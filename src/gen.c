#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "scope.h"
#include "types.h"
#include "util.h"

static char *
gen_name(struct gen_context *ctx, const char *fmt)
{
	int n = snprintf(NULL, 0, fmt, ctx->id);
	char *str = xcalloc(1, n + 1);
	snprintf(str, n + 1, fmt, ctx->id);
	++ctx->id;
	return str;
}

// Initializes a qval with a qbe temporary for a given gen temporary.
static void
qval_temp(struct gen_context *ctx,
	struct qbe_value *out,
	const struct gen_temp *temp)
{
	out->kind = QV_TEMPORARY;
	out->type = qtype_lookup(ctx, temp->type, true);
	out->name = temp->name;
}

// Initializes a qbe_value as a qbe temporary for the given qbe type.
static void
gen_qtemp(struct gen_context *ctx, struct qbe_value *out,
		const struct qbe_type *type, const char *fmt)
{
	out->kind = QV_TEMPORARY;
	out->type = type;
	out->name = gen_name(ctx, fmt);
}

// Generates a direct temporary of the given type, which must be a primitive
// type.
static void
gen_direct(struct gen_context *ctx, struct gen_temp *temp,
		const struct type *type, const char *fmt)
{
	assert(type->size != 0 && type->size != SIZE_UNDEFINED);
	temp->type = type;
	temp->name = gen_name(ctx, fmt);
	temp->indirect = false;
	const struct qbe_type *qtype = qtype_lookup(ctx, type, false);
	assert(qtype->stype != Q__AGGREGATE && qtype->stype != Q__VOID);
}

// Allocates a temporary of the given type on the stack in the current
// function's preamble.
static void
alloc_temp(struct gen_context *ctx, struct gen_temp *temp,
		const struct type *type, const char *fmt)
{
	assert(type->size != 0 && type->size != SIZE_UNDEFINED);
	temp->type = type;
	temp->name = gen_name(ctx, fmt);
	temp->indirect = true;

	struct qbe_value out = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = temp->name,
	};
	struct qbe_value size;
	constl(&size, type->size);
	pushprei(ctx->current, &out, alloc_for_align(type->align), &size, NULL);
}

// Loads a gen temporary into a qbe temporary. For types representable in qbe's
// type system, this loads the actual value into a qbe temporary. Otherwise,
// this behaves equivalently to qval_temp, but sets the temporary type to the
// platform's pointer type (e.g. =l).
static void
load_temp(struct gen_context *ctx,
	struct qbe_value *out,
	const struct gen_temp *temp)
{
	const struct qbe_type *qtype = qtype_lookup(ctx, temp->type, true);
	assert(qtype->stype != Q__VOID);

	out->kind = QV_TEMPORARY;
	if (qtype->stype == Q__AGGREGATE) {
		assert(temp->indirect);
		out->name = temp->name;
		out->type = ctx->arch.ptr;
	} else {
		out->name = gen_name(ctx, "load.%d");
		out->type = qtype;

		struct qbe_value src;
		qval_temp(ctx, &src, temp);
		if (temp->indirect) {
			enum qbe_instr instr = load_for_type(ctx, temp->type);
			pushi(ctx->current, out, instr, &src, NULL);
		} else {
			pushi(ctx->current, out, Q_COPY, &src, NULL);
		}
	}
}

static const struct gen_binding *
binding_lookup(struct gen_context *ctx, const struct scope_object *obj)
{
	for (struct gen_binding *binding = ctx->bindings;
			binding; binding = binding->next) {
		if (binding->object == obj) {
			return binding;
		}
	}
	abort(); // Invariant
}

static void
gen_copy_memcpy(struct gen_context *ctx,
	const struct gen_temp *dest,
	const struct gen_temp *src)
{
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
	const struct type *atype = type_dealias(dest->type);
	assert(atype->storage == STORAGE_ARRAY);
	assert(atype->array.length != SIZE_UNDEFINED);
	if (atype->array.length > 8) {
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
	const struct type *stype = type_dealias(dest->type);
	assert(stype->storage == STORAGE_STRUCT);
	if (stype->size > 32) {
		gen_copy_memcpy(ctx, dest, src);
		return;
	}
	// TODO: Generate more efficient approach
	gen_copy_memcpy(ctx, dest, src);
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
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_ENUM:
	case STORAGE_F32:
	case STORAGE_F64:
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
	case STORAGE_ARRAY:
		gen_copy_array(ctx, dest, src);
		return;
	case STORAGE_STRUCT:
		gen_copy_struct(ctx, dest, src);
		return;
	case STORAGE_UNION:
		gen_copy_memcpy(ctx, dest, src);
		return;
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		assert(0); // TODO
	case STORAGE_ALIAS:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_VOID:
		abort(); // Invariant
	}

	// Copy between types which have a native qbe representation
	struct qbe_value value = {0}, dtemp = {0};
	load_temp(ctx, &value, src);
	qval_temp(ctx, &dtemp, dest);

	if (dest->indirect) {
		enum qbe_instr instr = store_for_type(ctx, dtype);
		pushi(ctx->current, NULL, instr, &value, &dtemp, NULL);
	} else {
		pushi(ctx->current, &dtemp, Q_COPY, &value, NULL);
	}
}

static void gen_expr(struct gen_context *ctx,
	const struct expression *expr, const struct gen_temp *out);

static void gen_access_address(struct gen_context *ctx,
	struct gen_temp *temp, const struct expression *expr);

static void
gen_address_object(struct gen_context *ctx, struct gen_temp *temp,
	const struct scope_object *obj)
{
	const struct gen_binding *binding = binding_lookup(ctx, obj);
	assert(binding->temp.indirect);
	*temp = binding->temp;
}

static void
gen_address_field(struct gen_context *ctx, struct gen_temp *temp,
	const struct expression_access *access)
{
	assert(access->type == ACCESS_FIELD);

	const struct expression *object = access->_struct;
	assert(object->type == EXPR_ACCESS); // TODO: Other cases?

	struct gen_temp base;
	struct qbe_value qbase = {0}, field = {0}, offset = {0};
	gen_access_address(ctx, &base, object);
	qval_temp(ctx, &qbase, &base);
	gen_qtemp(ctx, &field, ctx->arch.ptr, "field.%d");
	constl(&offset, access->field->offset);
	pushi(ctx->current, &field, Q_ADD, &qbase, &offset, NULL);
	temp->name = field.name;
	temp->type = access->field->type;
}

static void
gen_address_index(struct gen_context *ctx, struct gen_temp *temp,
	const struct expression_access *access)
{
	assert(access->type == ACCESS_INDEX);

	const struct type *atype = access->array->result;
	const struct expression *object = access->array;
	assert(object->type == EXPR_ACCESS); // TODO: Other cases?

	struct gen_temp base;
	gen_access_address(ctx, &base, object);

	struct gen_temp index;
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
	struct gen_temp src;
	gen_access_address(ctx, &src, expr);
	gen_copy(ctx, out, &src);
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

	assert(!expr->assign.indirect); // TODO
	assert(object->type == EXPR_ACCESS); // Invariant

	struct gen_temp obj;
	gen_access_address(ctx, &obj, object);
	gen_expr(ctx, value, &obj);
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
gen_expr_const_array(struct gen_context *ctx,
		const struct type *atype,
		const struct array_constant *expr,
		const struct gen_temp *out)
{
	assert(!expr->expand); // TODO

	struct qbe_value base = {0}, ptr = {0}, membsz = {0};
	qval_temp(ctx, &base, out);
	gen_qtemp(ctx, &ptr, ctx->arch.ptr, "offset.%d");
	constl(&membsz, atype->array.members->size);
	pushi(ctx->current, &ptr, Q_COPY, &base, NULL);

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
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
	case STORAGE_NULL:
	case STORAGE_SLICE:
	case STORAGE_STRING:
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
	if (!out) {
		pushc(ctx->current, "Useless struct expression dropped");
		return;
	}
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
	case EXPR_ASSERT:
		assert(0); // TODO
	case EXPR_ASSIGN:
		gen_expr_assign(ctx, expr, out);
		break;
	case EXPR_BINARITHM:
		assert(0); // TODO
	case EXPR_BINDING:
		gen_expr_binding(ctx, expr, out);
		break;
	case EXPR_BREAK:
	case EXPR_CONTINUE:
	case EXPR_CALL:
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
	case EXPR_UNARITHM:
		assert(0); // TODO
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
