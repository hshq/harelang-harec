#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "scope.h"
#include "types.h"
#include "util.h"

static void
gen_auto_deref(struct gen_context *ctx, struct gen_temp *val)
{
	const struct type *type = val->type;
	if (!val->indirect && type_dealias(type)->storage == STORAGE_POINTER) {
		// We get one free dereference in this case
		type = type_dealias(type)->pointer.referent;
	}
	struct qbe_value qval = {0};
	qval_temp(ctx, &qval, val);
	while (type_dealias(type)->storage == STORAGE_POINTER) {
		type = type_dealias(type)->pointer.referent;
		// XXX: On some obsolete architectures, uintptr and pointer are
		// not necessarily the same representation.
		pushi(ctx->current, &qval,
			load_for_type(ctx, &builtin_type_uintptr),
			&qval, NULL);
	}
	val->type = type;
	val->indirect = true;
}

static void
gen_copy_memcpy(struct gen_context *ctx,
	const struct gen_temp *dest,
	const struct gen_temp *src)
{
	assert(dest->indirect);
	assert(src->indirect);
	struct qbe_value rtfunc = {0}, size = {0};
	rtfunc.kind = QV_GLOBAL;
	rtfunc.name = strdup("rt.memcpy");
	rtfunc.type = &qbe_long;
	constl(&size, dest->type->size);
	struct qbe_value dtemp = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = dest->name,
	}, stemp = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = src->name,
	};
	pushi(ctx->current, NULL, Q_CALL, &rtfunc,
			&dtemp, &stemp, &size, NULL);
}

static void
gen_copy_array(struct gen_context *ctx,
	const struct gen_temp *dest,
	const struct gen_temp *src)
{
	assert(dest->indirect);
	const struct type *atype = type_dealias(dest->type);
	assert(atype->storage == STORAGE_ARRAY);
	assert(atype->array.length != SIZE_UNDEFINED);
	if (atype->size > 128) {
		gen_copy_memcpy(ctx, dest, src);
		return;
	}
	// TODO: Generate more efficient approach
	gen_copy_memcpy(ctx, dest, src);
}

static void
gen_copy_struct(struct gen_context *ctx,
	const struct gen_temp *dest,
	const struct gen_temp *src)
{
	assert(dest->indirect);
	const struct type *stype = type_dealias(dest->type);
	assert(stype->storage == STORAGE_STRUCT);
	if (stype->size > 128) {
		gen_copy_memcpy(ctx, dest, src);
		return;
	}
	// TODO: Generate more efficient approach
	gen_copy_memcpy(ctx, dest, src);
}

static void
gen_copy_string(struct gen_context *ctx,
	const struct gen_temp *dest,
	const struct gen_temp *src)
{
	assert(dest->indirect && src->indirect);
	const struct type *voidptr = type_store_lookup_pointer(ctx->store,
			&builtin_type_void, 0);
	enum qbe_instr store = store_for_type(ctx, voidptr);
	enum qbe_instr load = load_for_type(ctx, voidptr);

	struct qbe_value dptr = {0}, sptr = {0}, temp = {0}, offset = {0};
	temp_workcopy(ctx, &dptr, ctx->arch.ptr, dest, "dptr.%d");
	temp_workcopy(ctx, &sptr, ctx->arch.ptr, src, "sptr.%d");
	gen_qtemp(ctx, &temp, ctx->arch.ptr, "temp.%d");
	constl(&offset, voidptr->size);

	// Data
	pushi(ctx->current, &temp, load, &sptr, NULL);
	pushi(ctx->current, NULL, store, &dptr, &temp, NULL);
	// Length
	pushi(ctx->current, &dptr, Q_ADD, &dptr, &offset, NULL);
	pushi(ctx->current, &sptr, Q_ADD, &sptr, &offset, NULL);
	pushi(ctx->current, &temp, load, &sptr, NULL);
	pushi(ctx->current, NULL, store, &dptr, &temp, NULL);
	// Capacity
	pushi(ctx->current, &dptr, Q_ADD, &dptr, &offset, NULL);
	pushi(ctx->current, &sptr, Q_ADD, &sptr, &offset, NULL);
	pushi(ctx->current, &temp, load, &sptr, NULL);
	pushi(ctx->current, NULL, store, &dptr, &temp, NULL);
}

// Generates a copy operation from one gen temporary to another. For primitive
// types this is a load+store operation; for aggregate types this may emit more
// complex code or a memcpy.
static void
gen_copy(struct gen_context *ctx,
	const struct gen_temp *dest,
	const struct gen_temp *src)
{
	const struct type *dtype = type_dealias(dest->type);
	assert(dtype == type_dealias(src->type));
	switch (dtype->storage) {
	case STORAGE_ARRAY:
		gen_copy_array(ctx, dest, src);
		return;
	case STORAGE_STRUCT:
		gen_copy_struct(ctx, dest, src);
		return;
	case STORAGE_UNION:
		gen_copy_memcpy(ctx, dest, src);
		return;
	case STORAGE_STRING:
		gen_copy_string(ctx, dest, src);
		return;
	case STORAGE_SLICE:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		assert(0); // TODO
	case STORAGE_ALIAS:
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_VOID:
		abort(); // Invariant
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_ENUM:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FUNCTION:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_INT:
	case STORAGE_NULL:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
		// Implemented below
		break;
	}

	// Copy between types which have a native qbe representation
	struct qbe_value value = {0};
	load_temp(ctx, &value, src);
	store_temp(ctx, dest, &value);
}

static void gen_expr(struct gen_context *ctx,
	const struct expression *expr, const struct gen_temp *out);

static void gen_access_address(struct gen_context *ctx,
	struct gen_temp *temp, const struct expression *expr);

static void
gen_address_object(struct gen_context *ctx, struct gen_temp *temp,
	const struct scope_object *obj)
{
	const struct gen_binding *binding = NULL;
	switch (obj->otype) {
	case O_BIND:
		binding = binding_lookup(ctx, obj);
		assert(binding->temp.indirect);
		*temp = binding->temp;
		return;
	case O_DECL:
		temp->is_global = true;
		temp->indirect = false;
		temp->type = obj->type;
		temp->name = ident_to_sym(&obj->ident);
		return;
	case O_CONST:
	case O_TYPE:
		abort(); // Invariant
	}
	abort();
}

static void
gen_address_field(struct gen_context *ctx, struct gen_temp *temp,
	const struct expression_access *access)
{
	assert(access->type == ACCESS_FIELD);

	const struct expression *object = access->_struct;
	assert(object->type == EXPR_ACCESS); // TODO: Other cases?

	struct gen_temp base = {0};
	struct qbe_value qbase = {0}, field = {0}, offset = {0};
	gen_access_address(ctx, &base, object);
	gen_auto_deref(ctx, &base);
	qval_temp(ctx, &qbase, &base);
	gen_qtemp(ctx, &field, ctx->arch.ptr, "field.%d");
	constl(&offset, access->field->offset);
	pushi(ctx->current, &field, Q_ADD, &qbase, &offset, NULL);
	temp->name = field.name;
	temp->type = access->field->type;
	temp->indirect = true;
}

static void
gen_address_index(struct gen_context *ctx, struct gen_temp *temp,
	const struct expression_access *access)
{
	assert(access->type == ACCESS_INDEX);

	const struct type *atype = type_dereference(access->array->result);
	assert(atype->storage == STORAGE_ARRAY);
	const struct expression *object = access->array;
	assert(object->type == EXPR_ACCESS); // TODO: Other cases?

	struct gen_temp base = {0};
	gen_access_address(ctx, &base, object);
	gen_auto_deref(ctx, &base);

	struct gen_temp index = {0};
	gen_direct(ctx, &index, &builtin_type_size, "index.%d");
	gen_expr(ctx, access->index, &index);

	// TODO: Check bounds

	struct qbe_value qbase = {0}, membsz = {0}, offset = {0}, qindex = {0};
	constl(&membsz, atype->array.members->size);
	qval_temp(ctx, &qindex, &index);
	pushi(ctx->current, &qindex, Q_MUL, &qindex, &membsz, NULL);

	qval_temp(ctx, &qbase, &base);
	gen_qtemp(ctx, &offset, ctx->arch.ptr, "offset.%d");
	pushi(ctx->current, &offset, Q_ADD, &qbase, &qindex, NULL);

	temp->name = offset.name;
	temp->type = atype->array.members;
	temp->indirect = true;
}

static void
gen_access_address(struct gen_context *ctx, struct gen_temp *temp,
		const struct expression *expr)
{
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		gen_address_object(ctx, temp, expr->access.object);
		break;
	case ACCESS_INDEX:
		gen_address_index(ctx, temp, &expr->access);
		break;
	case ACCESS_FIELD:
		gen_address_field(ctx, temp, &expr->access);
		break;
	case ACCESS_TUPLE:
		assert(0); // TODO
	}
}

static void
gen_expr_access(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	struct gen_temp src = {0};
	gen_access_address(ctx, &src, expr);
	gen_copy(ctx, out, &src);
}

static void
gen_expr_assert(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	assert(expr->assert.message); // Invariant
	if (expr->assert.is_static) {
		return;
	}

	struct qbe_statement failedl = {0}, passedl = {0};
	struct qbe_value bfailed = {0}, bpassed = {0};
	struct qbe_value rtfunc = {0};
	rtfunc.kind = QV_GLOBAL;
	rtfunc.name = strdup("rt.abort");
	rtfunc.type = &qbe_long;

	struct gen_temp msg = {0};
	alloc_temp(ctx, &msg, &builtin_type_str, "abortstr.%d");

	if (expr->assert.cond) {
		bfailed.kind = QV_LABEL;
		bfailed.name = strdup(genl(&failedl, &ctx->id, "failed.%d"));
		bpassed.kind = QV_LABEL;
		bpassed.name = strdup(genl(&passedl, &ctx->id, "passed.%d"));

		struct gen_temp cond = {0};
		gen_direct(ctx, &cond, &builtin_type_bool, "cond.%d");
		gen_expr(ctx, expr->assert.cond, &cond);

		struct qbe_value qcond = {0};
		qval_temp(ctx, &qcond, &cond);
		pushi(ctx->current, NULL, Q_JNZ, &qcond, &bpassed, &bfailed, NULL);
		push(&ctx->current->body, &failedl);
		gen_expr(ctx, expr->assert.message, &msg);
	} else {
		gen_expr(ctx, expr->assert.message, &msg);
	}

	struct qbe_value qmsg = {0};
	qval_temp(ctx, &qmsg, &msg);
	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &qmsg, NULL);

	if (expr->assert.cond) {
		push(&ctx->current->body, &passedl);
	}
}

static void
gen_expr_assign(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	const struct expression *object = expr->assign.object;
	const struct expression *value = expr->assign.value;
	if (object->type == EXPR_SLICE) {
		assert(0); // TODO
	}

	assert(object->type == EXPR_ACCESS); // Invariant

	struct gen_temp obj = {0};
	gen_access_address(ctx, &obj, object);
	if (expr->assign.indirect) {
		struct gen_temp temp = {0};
		gen_direct(ctx, &temp, object->result, "assign.%d");

		struct qbe_value qtemp, otemp;
		qval_temp(ctx, &qtemp, &temp);
		qval_temp(ctx, &otemp, &obj);
		enum qbe_instr instr = load_for_type(ctx, object->result);
		pushi(ctx->current, &qtemp, instr, &otemp, NULL);

		temp.indirect = true;
		temp.type = type_dereference(object->result);
		gen_expr(ctx, value, &temp);
	} else {
		gen_expr(ctx, value, &obj);
	}
}

static void
gen_expr_binarithm(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	assert(expr->binarithm.op != BIN_LAND && expr->binarithm.op != BIN_LOR); // TODO
	assert(!type_is_aggregate(expr->result)); // TODO

	const struct expression *lvexpr = expr->binarithm.lvalue;
	const struct expression *rvexpr = expr->binarithm.rvalue;

	struct qbe_value lvalue, rvalue;
	gen_qtemp(ctx, &lvalue,
		qtype_lookup(ctx, lvexpr->result, false), "lvalue.%d");
	gen_qtemp(ctx, &rvalue,
		qtype_lookup(ctx, rvexpr->result, false), "rvalue.%d");

	struct gen_temp lvg = {
		.name = lvalue.name,
		.type = lvexpr->result,
		.indirect = false,
	}, rvg = {
		.name = rvalue.name,
		.type = rvexpr->result,
		.indirect = false,
	};
	gen_expr(ctx, lvexpr, &lvg);
	gen_expr(ctx, rvexpr, &rvg);

	enum qbe_instr instr = binarithm_for_op(
		ctx, expr->binarithm.op, lvexpr->result);
	struct qbe_value result;
	gen_qtemp(ctx, &result, lvalue.type, "result.%d");
	pushi(ctx->current, &result, instr, &lvalue, &rvalue, NULL);
	store_temp(ctx, out, &result);
}

static void
gen_expr_binding(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	for (const struct expression_binding *binding = &expr->binding;
			binding; binding = binding->next) {
		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		alloc_temp(ctx, &gb->temp, binding->object->type, "binding.%d");
		gb->object = binding->object;
		pushc(ctx->current, "binding %s => %s",
				binding->object->ident.name,
				gb->temp.name);

		gen_expr(ctx, binding->initializer, &gb->temp);
		gb->next = ctx->bindings;
		ctx->bindings = gb;
	}
}

static void
gen_expr_call(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	struct gen_temp lvalue = {0};
	gen_direct(ctx, &lvalue, expr->call.lvalue->result, "call.lvalue.%d");
	gen_expr(ctx, expr->call.lvalue, &lvalue);
	gen_auto_deref(ctx, &lvalue);

	const struct type *rtype = lvalue.type;
	assert(rtype->storage == STORAGE_FUNCTION);
	// TODO: Run deferred expressions if rtype->func.flags & FN_NORETURN
	struct qbe_statement call = {
		.type = Q_INSTR,
		.instr = Q_CALL,
	};
	if (out) {
		call.out = xcalloc(1, sizeof(struct qbe_value));
		gen_qtemp(ctx, call.out, qtype_lookup(ctx, expr->result, true),
			"call.returns.%d");
	}

	struct qbe_arguments *args, **next = &call.args;
	struct call_argument *carg = expr->call.args;
	args = *next = xcalloc(1, sizeof(struct qbe_arguments));
	qval_temp(ctx, &args->value, &lvalue);
	next = &args->next;
	while (carg) {
		args = *next = xcalloc(1, sizeof(struct qbe_arguments));
		struct gen_temp arg = {0};
		if (type_is_aggregate(carg->value->result)) {
			alloc_temp(ctx, &arg, carg->value->result, "call.arg.%d");
		} else {
			gen_direct(ctx, &arg, carg->value->result, "call.arg.%d");
		}

		gen_expr(ctx, carg->value, &arg);
		qval_temp(ctx, &args->value, &arg);
		carg = carg->next;
		next = &args->next;
	}

	push(&ctx->current->body, &call);

	if (out) {
		struct gen_temp returns = {
			.name = call.out->name,
			.type = expr->result,
			.is_global = false,
			.indirect = false,
		};
		if (type_is_aggregate(expr->result)) {
			returns.indirect = true;
		}
		gen_copy(ctx, out, &returns);
	}
}

static void
gen_expr_const_array(struct gen_context *ctx,
		const struct type *atype,
		const struct array_constant *expr,
		const struct gen_temp *out)
{
	assert(!expr->expand); // TODO
	assert(out); // TODO: Ensure side-effects occur

	struct qbe_value ptr = {0}, membsz = {0};
	temp_workcopy(ctx, &ptr, ctx->arch.ptr, out, "offset.%d");
	constl(&membsz, atype->array.members->size);

	for (const struct array_constant *ac = expr; ac; ac = ac->next) {
		struct gen_temp temp = {
			.type = atype->array.members,
			.name = ptr.name,
			.indirect = true,
		};
		gen_expr(ctx, ac->value, &temp);

		if (ac->next) {
			pushi(ctx->current, &ptr, Q_ADD, &ptr, &membsz, NULL);
		}
	}
}

static void
gen_expr_const_string(struct gen_context *ctx,
		size_t length, const char *value,
		const struct gen_temp *out)
{
	struct qbe_value global = {0};
	gen_qtemp(ctx, &global, ctx->arch.ptr, "strdata.%d");
	global.kind = QV_GLOBAL;

	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->name = global.name;
	def->kind = Q_DATA;
	def->data.items.type = QD_STRING;
	def->data.items.str = xcalloc(1, length);
	memcpy(def->data.items.str, value, length);
	def->data.items.sz = length;

	if (length != 0) {
		qbe_append_def(ctx->out, def);
	} else {
		free(def);
		constl(&global, 0);
	}

	assert(out->indirect); // Invariant
	struct qbe_value temp = {0};
	temp_workcopy(ctx, &temp, ctx->arch.ptr, out, "str.%d");

	struct qbe_value offset = {0}, qlength = {0};
	const struct type *voidptr = type_store_lookup_pointer(ctx->store,
			&builtin_type_void, 0);
	pushi(ctx->current, NULL, store_for_type(ctx, voidptr),
		&global, &temp, NULL);
	constl(&offset, voidptr->size);
	constl(&qlength, length);
	pushi(ctx->current, &temp, Q_ADD, &temp, &offset, NULL);
	pushi(ctx->current, NULL, store_for_type(ctx, &builtin_type_size),
		&qlength, &temp, NULL);
	pushi(ctx->current, &temp, Q_ADD, &temp, &offset, NULL);
	pushi(ctx->current, NULL, store_for_type(ctx, &builtin_type_size),
		&qlength, &temp, NULL);
}

static void
gen_expr_constant(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	if (out == NULL) {
		pushc(ctx->current, "Useless constant expression dropped");
		return;
	}
	const struct expression_constant *constexpr = &expr->constant;
	assert(constexpr->object == NULL); // TODO

	struct qbe_value qout, qval = {0};
	qval_temp(ctx, &qout, out);

	enum type_storage storage = type_dealias(expr->result)->storage;
	if (storage == STORAGE_ENUM) {
		storage = type_dealias(expr->result)->_enum.storage;
	}

	switch (storage) {
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
	case STORAGE_I16:
	case STORAGE_U16:
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_RUNE:
	case STORAGE_BOOL:
		constw(&qval, constexpr->uval);
		break;
	case STORAGE_I64:
	case STORAGE_U64:
		constl(&qval, constexpr->uval);
		break;
	case STORAGE_F32:
		consts(&qval, constexpr->fval);
		break;
	case STORAGE_F64:
		constd(&qval, constexpr->fval);
		break;
	case STORAGE_SIZE:
		switch (ctx->arch.sz->size) {
		case 8:
			constl(&qval, constexpr->uval);
			break;
		default:
			abort();
		}
		break;
	case STORAGE_ARRAY:
		gen_expr_const_array(ctx, type_dealias(expr->result),
				constexpr->array, out);
		return;
	case STORAGE_STRING:
		gen_expr_const_string(ctx, constexpr->string.len,
				constexpr->string.value, out);
		return;
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
	case STORAGE_NULL:
	case STORAGE_SLICE:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		assert(0); // TODO
	case STORAGE_ICONST:
	case STORAGE_FCONST:
	case STORAGE_ENUM:
	case STORAGE_VOID:
	case STORAGE_ALIAS:
	case STORAGE_FUNCTION:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		abort(); // Invariant
	}

	if (out->indirect) {
		enum qbe_instr instr = store_for_type(ctx, expr->result);
		pushi(ctx->current, NULL, instr, &qval, &qout, NULL);
	} else {
		pushi(ctx->current, &qout, Q_COPY, &qval, NULL);
	}
}

static void
gen_expr_list(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	for (const struct expressions *item = &expr->list.exprs;
			item; item = item->next) {
		if (!item->next) {
			gen_expr(ctx, item->expr, out);
		} else {
			gen_expr(ctx, item->expr, NULL);
		}
	}
}

static void
gen_expr_return(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	struct qbe_value label = {
		.kind = QV_LABEL,
		.name = strdup(ctx->end),
	};
	if (expr->_return.value) {
		gen_expr(ctx, expr->_return.value, ctx->rval);
	}
	pushi(ctx->current, NULL, Q_JMP, &label, NULL);
}

static void
gen_expr_struct(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	assert(out); // TODO: Ensure side-effects occur
	struct qbe_value base = {0}, ptr = {0}, offs = {0};
	qval_temp(ctx, &base, out);
	gen_qtemp(ctx, &ptr, ctx->arch.ptr, "offset.%d");

	if (expr->_struct.autofill) {
		struct qbe_value rtfunc = {0}, size = {0}, zero = {0};
		rtfunc.kind = QV_GLOBAL;
		rtfunc.name = strdup("rt.memset");
		rtfunc.type = &qbe_long;
		constl(&size, expr->result->size);
		constl(&zero, 0);
		pushi(ctx->current, NULL, Q_CALL, &rtfunc,
				&base, &zero, &size, NULL);
	}

	const struct expr_struct_field *field = &expr->_struct.fields;
	while (field) {
		if (!field->value) {
			assert(expr->_struct.autofill);
			field = field->next;
			continue;
		}

		constl(&offs, field->field->offset);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);

		struct gen_temp temp = {
			.name = ptr.name,
			.type = field->field->type,
			.indirect = true,
		};
		gen_expr(ctx, field->value, &temp);
		field = field->next;
	}
}

static void
gen_expr_unarithm(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	assert(out); // TODO: Ensure side-effects occur

	struct gen_temp temp = {0};
	const struct expression *operand = expr->unarithm.operand;
	switch (expr->unarithm.op) {
	case UN_ADDRESS:
		assert(operand->type == EXPR_ACCESS);
		gen_access_address(ctx, &temp, operand);
		temp_address(&temp, out->type);
		gen_copy(ctx, out, &temp);
		break;
	case UN_DEREF:
		gen_direct(ctx, &temp, operand->result, "deref.%d");
		gen_expr(ctx, operand, &temp);
		temp_deref(&temp);
		gen_copy(ctx, out, &temp);
		break;
	case UN_BNOT:
	case UN_LNOT:
	case UN_MINUS:
	case UN_PLUS:
		assert(0); // TODO
	}
}

static void
gen_expr(struct gen_context *ctx,
		const struct expression *expr,
		const struct gen_temp *out)
{
	switch (expr->type) {
	case EXPR_ACCESS:
		gen_expr_access(ctx, expr, out);
		break;
	case EXPR_ALLOC:
	case EXPR_APPEND:
		assert(0); // TODO
	case EXPR_ASSERT:
		gen_expr_assert(ctx, expr, out);
		break;
	case EXPR_ASSIGN:
		gen_expr_assign(ctx, expr, out);
		break;
	case EXPR_BINARITHM:
		gen_expr_binarithm(ctx, expr, out);
		break;
	case EXPR_BINDING:
		gen_expr_binding(ctx, expr, out);
		break;
	case EXPR_BREAK:
	case EXPR_CONTINUE:
		assert(0); // TODO
	case EXPR_CALL:
		gen_expr_call(ctx, expr, out);
		break;
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		gen_expr_constant(ctx, expr, out);
		break;
	case EXPR_DEFER:
	case EXPR_DELETE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
	case EXPR_INSERT:
		assert(0); // TODO
	case EXPR_LIST:
		gen_expr_list(ctx, expr, out);
		break;
	case EXPR_MATCH:
	case EXPR_MEASURE:
		assert(0); // TODO
	case EXPR_PROPAGATE:
		assert(0); // Lowered in check (XXX: for now...)
	case EXPR_RETURN:
		gen_expr_return(ctx, expr, out);
		break;
	case EXPR_SLICE:
		assert(0); // TODO
	case EXPR_STRUCT:
		gen_expr_struct(ctx, expr, out);
		break;
	case EXPR_SWITCH:
	case EXPR_TUPLE:
		assert(0); // TODO
	case EXPR_UNARITHM:
		gen_expr_unarithm(ctx, expr, out);
		break;
	}
}

static void
gen_function_decl(struct gen_context *ctx, const struct declaration *decl)
{
	const struct function_decl *func = &decl->func;
	const struct type *fntype = func->type;
	if (func->body == NULL) {
		return; // Prototype
	}
	// TODO: Attributes
	assert(!func->flags);

	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_FUNC;
	qdef->exported = decl->exported;
	qdef->name = decl->symbol ? strdup(decl->symbol)
		: ident_to_sym(&decl->ident);
	ctx->current = &qdef->func;

	struct qbe_statement start_label = {0};
	struct qbe_statement end_label = {0};
	genl(&start_label, &ctx->id, "start.%d");
	ctx->end = genl(&end_label, &ctx->id, "end.%d");
	push(&qdef->func.prelude, &start_label);

	if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		ctx->rval = xcalloc(1, sizeof(struct gen_temp));
		alloc_temp(ctx, ctx->rval, fntype->func.result, "rval.%d");
		qdef->func.returns = qtype_lookup(ctx, fntype->func.result, false);
	} else {
		qdef->func.returns = &qbe_void;
	}

	struct qbe_func_param *param, **next = &qdef->func.params;
	struct scope_object *obj = decl->func.scope->objects;
	while (obj) {
		param = *next = xcalloc(1, sizeof(struct qbe_func_param));
		assert(!obj->ident.ns); // Invariant
		param->name = strdup(obj->ident.name);
		param->type = qtype_lookup(ctx, obj->type, false);

		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		if (type_is_aggregate(obj->type)) {
			gb->temp.name = strdup(param->name);
			gb->temp.type = obj->type;
			gb->temp.indirect = true;
		} else {
			alloc_temp(ctx, &gb->temp, obj->type, "parameter.%d");
			struct gen_temp temp = {
				.name = param->name,
				.type = obj->type,
				.indirect = false,
			};
			gen_copy(ctx, &gb->temp, &temp);
		}
		gb->object = obj;
		gb->next = ctx->bindings;
		ctx->bindings = gb;

		obj = obj->lnext;
		next = &param->next;
	}

	pushl(&qdef->func, &ctx->id, "body.%d");
	gen_expr(ctx, func->body, ctx->rval);

	push(&qdef->func.body, &end_label);
	if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		struct qbe_value rval = {0};
		load_temp(ctx, &rval, ctx->rval);
		pushi(ctx->current, NULL, Q_RET, &rval, NULL);
	} else {
		pushi(ctx->current, NULL, Q_RET, NULL);
	}

	qbe_append_def(ctx->out, qdef);
	ctx->current = NULL;
}

static void
gen_decl(struct gen_context *ctx, const struct declaration *decl)
{
	switch (decl->type) {
	case DECL_FUNC:
		gen_function_decl(ctx, decl);
		break;
	case DECL_GLOBAL:
		assert(0); // TODO
	case DECL_CONST:
	case DECL_TYPE:
		break; // Nothing to do
	}
}

void
gen(const struct unit *unit, struct type_store *store, struct qbe_program *out)
{
	struct gen_context ctx = {
		.out = out,
		.store = store,
		.ns = unit->ns,
		.arch = {
			.ptr = &qbe_long,
			.sz = &qbe_long,
		},
	};
	ctx.out->next = &ctx.out->defs;
	const struct declarations *decls = unit->declarations;
	while (decls) {
		gen_decl(&ctx, decls->decl);
		decls = decls->next;
	}
}
