#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "identifier.h"
#include "qbe.h"
#include "scope.h"
#include "trace.h"
#include "types.h"
#include "util.h"

static char *
ident_to_sym(const struct identifier *ident)
{
	if (ident->ns) {
		char *ns = ident_to_sym(ident->ns);
		if (!ns) {
			return NULL;
		}
		int n = snprintf(NULL, 0, "%s.%s", ns, ident->name);
		char *str = xcalloc(1, n + 1);
		snprintf(str, n + 1, "%s.%s", ns, ident->name);
		free(ns);
		return str;
	}
	return strdup(ident->name);
}

static void
gen_temp(struct gen_context *ctx, struct qbe_value *val,
		const struct qbe_type *type, const char *fmt)
{
	int n = snprintf(NULL, 0, fmt, ctx->id);
	char *str = xcalloc(1, n + 1);
	snprintf(str, n + 1, fmt, ctx->id);
	++ctx->id;

	val->kind = QV_TEMPORARY;
	val->type = type;
	val->name = str;
	val->indirect = false;
}

static void
alloc_temp(struct gen_context *ctx, struct qbe_value *val,
		const struct type *type, const char *fmt)
{
	struct qbe_value size = {0};
	gen_temp(ctx, val, qtype_for_type(ctx, type, true), fmt);
	val->indirect = true;
	constl(&size, type->size);
	const struct qbe_type *qtype = val->type;
	val->type = &qbe_long;
	pushprei(ctx->current, val, alloc_for_align(type->align), &size, NULL);
	val->type = qtype;
}

static struct gen_binding *
binding_alloc(struct gen_context *ctx, const struct scope_object *obj,
		struct qbe_value *val, const char *fmt)
{
	struct gen_binding *binding = xcalloc(1, sizeof(struct gen_binding));
	alloc_temp(ctx, val, obj->type, fmt);
	if (type_is_aggregate(obj->type)) {
		val->indirect = false;
	}
	binding->name = strdup(val->name);
	binding->object = obj;
	binding->next = ctx->bindings;
	ctx->bindings = binding;
	pushc(ctx->current, "alloc binding: %s -> %%%s, =%c, indirect: %d",
			obj->ident.name, binding->name,
			(char)val->type->stype, val->indirect);
	return binding;
}

static const struct gen_binding *
binding_lookup(const struct gen_context *ctx, const struct scope_object *obj)
{
	struct gen_binding *binding = ctx->bindings;
	while (binding) {
		if (binding->object == obj) {
			return binding;
		}
		binding = binding->next;
	}
	return NULL;
}

static void
qval_for_object(struct gen_context *ctx,
	struct qbe_value *val,
	const struct scope_object *obj)
{
	const struct gen_binding *binding = NULL;
	switch (obj->otype) {
	case O_BIND:
		binding = binding_lookup(ctx, obj);
		val->kind = QV_TEMPORARY;
		val->indirect = true;
		val->name = strdup(binding->name);
		break;
	case O_DECL:
		val->kind = QV_GLOBAL;
		val->indirect = false;
		val->name = ident_to_sym(&obj->ident);
		break;
	case O_CONST:
		assert(0); // Invariant (lowered in check)
	}

	if (type_is_aggregate(obj->type)) {
		val->type = qtype_for_type(ctx, obj->type, true);
		val->indirect = false;
	} else {
		val->type = &qbe_long; // XXX: ARCH
	}
}

static void
qval_deref(struct qbe_value *val)
{
	val->indirect = val->type->stype != Q__AGGREGATE;
}

static void
qval_address(struct qbe_value *val)
{
	val->indirect = val->type->stype == Q__AGGREGATE;
}

static void
gen_copy(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src)
{
	assert(!dest->indirect && !src->indirect);
	assert(dest->type == src->type);
	const struct qbe_field *field = &dest->type->fields;

	struct qbe_value temp = {0}, destp = {0}, srcp = {0}, size = {0};
	gen_temp(ctx, &temp, &qbe_long, "temp.%d");
	gen_temp(ctx, &destp, &qbe_long, "dest.%d");
	gen_temp(ctx, &srcp, &qbe_long, "src.%d");
	pushi(ctx->current, &destp, Q_COPY, dest, NULL);
	pushi(ctx->current, &srcp, Q_COPY, src, NULL);

	while (field) {
		temp.type = field->type;

		for (size_t i = field->count; i > 0; --i) {
			switch (field->type->stype) {
			case Q_BYTE:
			case Q_HALF:
			case Q_WORD:
			case Q_LONG:
			case Q_SINGLE:
			case Q_DOUBLE:
				// XXX: This may be broken for unsigned types b
				// and h
				pushi(ctx->current, &temp,
					load_for_type(field->type->stype, true),
					&srcp, NULL);
				pushi(ctx->current, NULL,
					store_for_type(field->type->stype),
					&temp, &destp, NULL);
				break;
			case Q__AGGREGATE:
				assert(0); // TODO
			case Q__VOID:
				assert(0); // Invariant
			}

			if (i > 1) {
				assert(field->type->size != 0);
				constl(&size, field->type->size);
				pushi(ctx->current, &destp, Q_ADD, &destp, &size, NULL);
				pushi(ctx->current, &srcp, Q_ADD, &srcp, &size, NULL);
			}
		}

		field = field->next;
	}
}

static void
gen_store(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src)
{
	if (!dest) {
		// no-op
		return;
	}

	assert(src->type->stype != Q__VOID
		&& dest->type->stype != Q__VOID); // Invariant

	// TODO: Revisit me (again)
	if (src->type->stype == Q__AGGREGATE) {
		if (src->indirect && dest->indirect) {
			pushi(ctx->current, NULL, Q_STOREL, src, dest, NULL); // XXX: ARCH
		} else if (!dest->indirect && dest->type->stype == Q__AGGREGATE) {
			gen_copy(ctx, dest, src);
		} else {
			pushi(ctx->current, dest, Q_COPY, src, NULL);
		}
		return;
	}

	assert(!src->indirect);
	if (dest->indirect) {
		pushi(ctx->current, NULL,
			store_for_type(src->type->stype),
			src, dest, NULL);
	} else {
		pushi(ctx->current, dest, Q_COPY, src, NULL);
	}
}

static void
gen_load(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src,
	bool is_signed)
{
	assert(src->type->stype != Q__VOID
		&& dest->type->stype != Q__VOID); // Invariant

	if (src->type->stype == Q__AGGREGATE) {
		assert(!src->indirect);
		if (dest->type->stype == Q__AGGREGATE) {
			assert(0); // TODO: memcpy
		} else {
			assert(dest->type == &qbe_long);
			pushi(ctx->current, dest, Q_COPY, src, NULL);
		}
		return;
	}

	assert(!dest->indirect);
	if (src->indirect) {
		pushi(ctx->current, dest,
			load_for_type(dest->type->stype, is_signed),
			src, NULL);
	} else {
		pushi(ctx->current, dest, Q_COPY, src, NULL);
	}
}

// Same as gen_load but dest is initialized to a new temporary
static void
gen_loadtemp(struct gen_context *ctx,
	struct qbe_value *dest, const struct qbe_value *src,
	const struct qbe_type *type, bool is_signed)
{
	gen_temp(ctx, dest, type, "load.%d");
	gen_load(ctx, dest, src, is_signed);
}

static void gen_expression(struct gen_context *ctx,
	const struct expression *expr, const struct qbe_value *out);

static void
address_ident(struct gen_context *ctx,
	const struct expression *expr,
	struct qbe_value *out)
{
	const struct scope_object *obj = expr->access.object;
	qval_for_object(ctx, out, obj);
}

static void
address_index(struct gen_context *ctx,
	const struct expression *expr,
	struct qbe_value *out)
{
	const struct type *atype = expr->access.array->result;
	while (atype->storage == TYPE_STORAGE_POINTER) {
		atype = atype->pointer.referent;
	}
	assert(atype->storage == TYPE_STORAGE_ARRAY); // TODO: Slices

	gen_temp(ctx, out, &qbe_long, "object.%d"); // XXX: ARCH
	gen_expression(ctx, expr->access.array, out);

	atype = expr->access.array->result;
	if (atype->storage == TYPE_STORAGE_POINTER) {
		// We get one dereference for free for aggregate types
		atype = atype->pointer.referent;
	}
	while (atype->storage == TYPE_STORAGE_POINTER) {
		pushi(ctx->current, out, Q_LOADL, out, NULL);
		atype = atype->pointer.referent;
	}

	struct qbe_value index = {0};
	gen_temp(ctx, &index, &qbe_long, "index.%d");
	gen_expression(ctx, expr->access.index, &index);

	// TODO: Check if offset is in bounds

	struct qbe_value temp = {0};
	constl(&temp, atype->array.members->size);
	pushi(ctx->current, &index, Q_MUL, &index, &temp, NULL);
	pushi(ctx->current, out, Q_ADD, out, &index, NULL);
	if (!type_is_aggregate(atype->array.members)) {
		qval_deref(out);
	}
}

static void
address_field(struct gen_context *ctx,
	const struct expression *expr,
	struct qbe_value *out)
{
	const struct type *stype = expr->access._struct->result;
	while (stype->storage == TYPE_STORAGE_POINTER) {
		stype = stype->pointer.referent;
	}

	gen_temp(ctx, out, &qbe_long, "object.%d"); // XXX: ARCH
	gen_expression(ctx, expr->access._struct, out);

	stype = expr->access._struct->result;
	if (stype->storage == TYPE_STORAGE_POINTER) {
		// We get one dereference for free for aggregate types
		stype = stype->pointer.referent;
	}
	while (stype->storage == TYPE_STORAGE_POINTER) {
		pushi(ctx->current, out, Q_LOADL, out, NULL);
		stype = stype->pointer.referent;
	}

	const struct struct_field *field = expr->access.field;

	struct qbe_value offset = {0};
	constl(&offset, field->offset);
	pushi(ctx->current, out, Q_ADD, out, &offset, NULL);
	if (!type_is_aggregate(field->type)) {
		qval_deref(out);
	}
}

static void
address_object(struct gen_context *ctx,
		const struct expression *expr,
		struct qbe_value *out)
{
	assert(expr->type == EXPR_ACCESS);
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		address_ident(ctx, expr, out);
		break;
	case ACCESS_INDEX:
		address_index(ctx, expr, out);
		break;
	case ACCESS_FIELD:
		address_field(ctx, expr, out);
		break;
	}
}

static void
gen_expr_access(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (out == NULL) {
		pushc(ctx->current, "useless access expression discarded");
		return;
	}

	struct qbe_value src = {0}, temp = {0};
	address_object(ctx, expr, &src);
	if (src.indirect) {
		gen_loadtemp(ctx, &temp, &src,
			qtype_for_type(ctx, expr->result, false),
			type_is_signed(expr->result));
		gen_store(ctx, out, &temp);
	} else {
		gen_store(ctx, out, &src);
	}
}

static void
gen_expr_assert(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(expr->assert.message); // Invariant

	struct qbe_statement failedl = {0}, passedl = {0};
	struct qbe_value bfailed = {0}, bpassed = {0};

	struct qbe_value msg = {0};

	struct qbe_value rtfunc = {0};
	rtfunc.kind = QV_GLOBAL;
	rtfunc.name = strdup("rt.abort");
	rtfunc.type = &qbe_long;

	if (expr->assert.cond) {
		bfailed.kind = QV_LABEL;
		bfailed.name = strdup(genl(&failedl, &ctx->id, "failed.%d"));
		bpassed.kind = QV_LABEL;
		bpassed.name = strdup(genl(&passedl, &ctx->id, "passed.%d"));

		struct qbe_value cond = {0};
		gen_temp(ctx, &cond, &qbe_word, "assert.%d");
		gen_expression(ctx, expr->assert.cond, &cond);

		alloc_temp(ctx, &msg, &builtin_type_const_str, "msg.%d");
		qval_deref(&msg);
		gen_expression(ctx, expr->assert.message, &msg);

		pushi(ctx->current, NULL, Q_JNZ, &cond, &bpassed, &bfailed, NULL);
		push(&ctx->current->body, &failedl);
	} else {
		alloc_temp(ctx, &msg, &builtin_type_const_str, "msg.%d");
		qval_deref(&msg);
		gen_expression(ctx, expr->assert.message, &msg);
	}

	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &msg, NULL);

	if (expr->assert.cond) {
		push(&ctx->current->body, &passedl);
	}
}

static void
gen_expr_assign(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL); // Invariant

	struct expression *object = expr->assign.object;
	assert(object->type == EXPR_ACCESS || expr->assign.indirect); // Invariant

	struct qbe_value src = {0};
	if (expr->assign.indirect) {
		gen_temp(ctx, &src, &qbe_long, "indirect.%d"); // XXX: ARCH
		gen_expression(ctx, object, &src);
		qval_deref(&src);
	} else {
		const struct expression *object = expr->assign.object;
		address_object(ctx, object, &src);
	}

	const struct expression *value = expr->assign.value;
	const struct type *objtype = expr->assign.indirect
		? object->result->pointer.referent : object->result;
	const struct qbe_type *vtype =
		qtype_for_type(ctx, value->result, false);
	const struct qbe_type *otype = qtype_for_type(ctx, objtype, false);

	if (expr->assign.op == BIN_LEQUAL) {
		if (!type_is_aggregate(value->result)) {
			qval_deref(&src);
		}
		gen_expression(ctx, value, &src);
	} else {
		struct qbe_value v = {0};
		gen_temp(ctx, &v, vtype, "assign.value.%d");
		gen_expression(ctx, value, &v);

		struct qbe_value result;
		gen_temp(ctx, &result, otype, "assign.result.%d");

		struct qbe_value load;
		gen_loadtemp(ctx, &load, &src, otype,
			type_is_signed(objtype));
		pushi(ctx->current, &result,
			binarithm_for_op(expr->assign.op, otype,
				type_is_signed(objtype)),
			&load, &v, NULL);
		gen_store(ctx, &src, &result);
	}
}

static void
gen_expr_binding(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL);

	const struct expression_binding *binding = &expr->binding;
	while (binding) {
		struct qbe_value temp = {0};
		binding_alloc(ctx, binding->object, &temp, "binding.%d");
		gen_expression(ctx, binding->initializer, &temp);
		binding = binding->next;
	}
}

static void
extend(struct gen_context *ctx, struct qbe_value *v, const struct type *type)
{
	enum qbe_instr op;
	switch (type->size) {
	case 1:
		op = type_is_signed(type) ? Q_EXTSB : Q_EXTUB;
		break;
	case 2:
		op = type_is_signed(type) ? Q_EXTSH : Q_EXTUH;
		break;
	default:
		return;
	}

	struct qbe_value temp = {0};
	gen_temp(ctx, &temp, &qbe_word, "ext.%d");
	pushi(ctx->current, &temp, op, v, NULL);
	*v = temp;
}

static void
gen_expr_binarithm(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct qbe_type *ltype =
		qtype_for_type(ctx, expr->binarithm.lvalue->result, false);
	const struct qbe_type *rtype =
		qtype_for_type(ctx, expr->binarithm.rvalue->result, false);
	const struct qbe_type *etype = qtype_for_type(ctx, expr->result, false);
	assert(ltype == rtype); // TODO: Type promotion

	struct qbe_value lvalue = {0}, rvalue = {0}, result = {0};
	gen_temp(ctx, &lvalue, ltype, "lvalue.%d");
	gen_temp(ctx, &rvalue, rtype, "rvalue.%d");
	gen_temp(ctx, &result, etype, "result.%d");

	gen_expression(ctx, expr->binarithm.lvalue, &lvalue);
	gen_expression(ctx, expr->binarithm.rvalue, &rvalue);

	switch (expr->binarithm.op) {
	case BIN_GREATER:
	case BIN_GREATEREQ:
	case BIN_LEQUAL:
	case BIN_LESS:
	case BIN_LESSEQ:
	case BIN_NEQUAL:
		extend(ctx, &lvalue, expr->binarithm.lvalue->result);
		extend(ctx, &rvalue, expr->binarithm.rvalue->result);
		break;
	default:
		break;
	}

	pushi(ctx->current, &result,
		binarithm_for_op(expr->binarithm.op, ltype,
			type_is_signed(expr->binarithm.lvalue->result)),
		&lvalue, &rvalue, NULL);
	gen_store(ctx, out, &result);
}

static void
gen_expr_call(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value result = {0};

	struct qbe_statement call = {
		.type = Q_INSTR,
		.instr = Q_CALL,
	};

	if (expr->call.lvalue->result->func.result != &builtin_type_void) {
		gen_temp(ctx, &result, qtype_for_type(ctx,
			expr->call.lvalue->result->func.result, true), "returns.%d");
		call.out = qval_dup(&result);
	}

	struct qbe_arguments *arg, **next = &call.args;
	struct call_argument *carg = expr->call.args;
	arg = *next = xcalloc(1, sizeof(struct qbe_arguments));
	gen_temp(ctx, &arg->value, &qbe_long, "func.%d");
	gen_expression(ctx, expr->call.lvalue, &arg->value);
	next = &arg->next;

	while (carg) {
		assert(!carg->variadic); // TODO
		arg = *next = xcalloc(1, sizeof(struct qbe_arguments));
		if (type_is_aggregate(carg->value->result)) {
			alloc_temp(ctx, &arg->value,
				carg->value->result, "arg.%d");
			qval_deref(&arg->value);
		} else {
			gen_temp(ctx, &arg->value,
				qtype_for_type(ctx, carg->value->result, true),
				"arg.%d");
		}
		gen_expression(ctx, carg->value, &arg->value);
		carg = carg->next;
		next = &arg->next;
	}

	push(&ctx->current->body, &call);

	if (out) {
		gen_store(ctx, out, &result);
	}
}

static void
gen_expr_cast(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct type *to = expr->result,
	      *from = expr->cast.value->result;
	if (to->storage == from->storage) {
		gen_expression(ctx, expr->cast.value, out);
		return;
	}

	bool is_signed = type_is_signed(from);

	struct qbe_value in = {0}, result = {0};
	gen_temp(ctx, &result, qtype_for_type(ctx, to, false), "cast.out.%d");

	// Special case: str -> *const char
	if (to->storage == TYPE_STORAGE_POINTER
			&& to->pointer.referent->storage == TYPE_STORAGE_CHAR
			&& from->storage == TYPE_STORAGE_STRING) {
		alloc_temp(ctx, &in, from, "cast.in.%d");
		in.indirect = false;
		gen_expression(ctx, expr->cast.value, &in);
		in.type = &qbe_long;
		qval_deref(&in);
		gen_load(ctx, &result, &in, false);
		gen_store(ctx, out, &result);
		return;
	}

	gen_temp(ctx, &in, qtype_for_type(ctx, from, false), "cast.in.%d");
	gen_expression(ctx, expr->cast.value, &in);

	enum qbe_instr op;
	switch (to->storage) {
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_INT:		// XXX: ARCH
	case TYPE_STORAGE_UINT:		// XXX: ARCH
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINTPTR:	// XXX: ARCH
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:		// XXX: ARCH
		if (type_is_integer(to) && to->size <= from->size) {
			op = Q_COPY;
		} else if (type_is_integer(to) && to->size > from->size) {
			switch (from->size) {
			case 4:
				op = is_signed ? Q_EXTSW : Q_EXTUW;
				break;
			case 2:
				op = is_signed ? Q_EXTSH : Q_EXTUH;
				break;
			case 1:
				op = is_signed ? Q_EXTSB : Q_EXTUB;
				break;
			default:
				assert(0); // Invariant
			}
		} else if (from->storage == TYPE_STORAGE_POINTER) {
			assert(to->storage == TYPE_STORAGE_UINTPTR);
			op = Q_COPY;
		} else if (type_is_float(from)) {
			assert(0); // TODO
		} else {
			assert(0); // Invariant
		}
		pushi(ctx->current, &result, op, &in, NULL);
		break;
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	// Can be implemented with a copy
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
		pushi(ctx->current, &result, Q_COPY, &in, NULL);
		break;
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		assert(0); // Invariant
	case TYPE_STORAGE_VOID:
		return; // no-op
	}

	gen_store(ctx, out, &result);
}

static void
gen_array(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct type *type = expr->result;
	assert(!type->array.expandable); // Invariant

	// XXX: ARCH
	struct qbe_value ptr = {0};
	gen_temp(ctx, &ptr, &qbe_long, "ptr.%d");
	pushi(ctx->current, &ptr, Q_COPY, out, NULL);
	ptr.indirect = !type_is_aggregate(type->array.members);

	struct qbe_value size = {0};
	constl(&size, type->array.members->size);

	struct array_constant *item = expr->constant.array;
	while (item) {
		gen_expression(ctx, item->value, &ptr);
		if (item->next) {
			pushi(ctx->current, &ptr, Q_ADD, &ptr, &size, NULL);
		}
		item = item->next;
	}
}

static void
gen_string(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(!out->indirect); // Invariant

	struct qbe_value temp = {0};
	gen_temp(ctx, &temp, &qbe_long, "strdata.%d");
	temp.kind = QV_GLOBAL;

	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->name = temp.name;
	def->kind = Q_DATA;
	def->data.items.type = QD_STRING;
	def->data.items.str = strdup(expr->constant.string.value);
	def->data.items.sz = expr->constant.string.len;
	qbe_append_def(ctx->out, def);

	struct qbe_value str = {0};
	gen_temp(ctx, &str, &qbe_long, "str.%d"); // XXX: ARCH
	pushi(ctx->current, &str, Q_COPY, out, NULL);
	str.indirect = true;

	struct qbe_value size = {0};
	constl(&size, expr->constant.string.len); // XXX: ARCH

	gen_store(ctx, &str, &temp);
	constl(&temp, 8); // XXX: ARCH
	pushi(ctx->current, &str, Q_ADD, &str, &temp, NULL);
	gen_store(ctx, &str, &size);
	pushi(ctx->current, &str, Q_ADD, &str, &temp, NULL);
	gen_store(ctx, &str, &size);
}

static void
gen_expr_constant(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (out == NULL) {
		pushc(ctx->current, "useless constant expression discarded");
		return;
	}

	struct qbe_value val = {0};

	// Special cases
	switch (expr->result->storage) {
	case TYPE_STORAGE_BOOL:
		constw(&val, expr->constant.bval ? 1 : 0);
		gen_store(ctx, out, &val);
		return;
	case TYPE_STORAGE_VOID:
		const_void(&val);
		gen_store(ctx, out, &val);
		return;
	case TYPE_STORAGE_NULL:
		constl(&val, 0);
		gen_store(ctx, out, &val);
		return;
	case TYPE_STORAGE_ARRAY:
		gen_array(ctx, expr, out);
		return;
	case TYPE_STORAGE_STRING:
		gen_string(ctx, expr, out);
		return;
	default:
		// Moving right along
		break;
	}

	const struct qbe_type *qtype = qtype_for_type(ctx, expr->result, false);
	switch (qtype->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
		constw(&val, (uint32_t)expr->constant.uval);
		break;
	case Q_LONG:
		constl(&val, (uint64_t)expr->constant.uval);
		break;
	case Q_SINGLE:
		consts(&val, (float)expr->constant.fval);
		break;
	case Q_DOUBLE:
		constd(&val, expr->constant.fval);
		break;
	case Q__AGGREGATE:
	case Q__VOID:
		assert(0); // Invariant
	}

	gen_store(ctx, out, &val);
}

static void
gen_expr_for(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL); // Invariant
	if (expr->_for.bindings) {
		gen_expr_binding(ctx, expr->_for.bindings, NULL);
	}

	struct qbe_statement loopl = {0}, bodyl = {0}, afterl = {0}, endl = {0};
	struct qbe_value loop = {0}, body = {0}, after = {0}, end = {0};
	loop.kind = QV_LABEL;
	loop.name = strdup(genl(&loopl, &ctx->id, "loop.%d"));
	body.kind = QV_LABEL;
	body.name = strdup(genl(&bodyl, &ctx->id, "body.%d"));
	after.kind = QV_LABEL;
	after.name = strdup(genl(&afterl, &ctx->id, "after.%d"));
	end.kind = QV_LABEL;
	end.name = strdup(genl(&endl, &ctx->id, "end.%d"));

	push(&ctx->current->body, &loopl);

	struct qbe_value cond = {0};
	gen_temp(ctx, &cond, &qbe_word, "cond.%d");
	gen_expression(ctx, expr->_for.cond, &cond);

	pushi(ctx->current, NULL, Q_JNZ, &cond, &body, &end, NULL);

	push(&ctx->current->body, &bodyl);
	gen_expression(ctx, expr->_for.body, NULL);

	push(&ctx->current->body, &afterl);
	if (expr->_for.afterthought) {
		gen_expression(ctx, expr->_for.afterthought, NULL);
	}
	(void)after; // TODO: continue

	pushi(ctx->current, NULL, Q_JMP, &loop, NULL);

	push(&ctx->current->body, &endl);
}

static void
gen_expr_if(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value cond = {0};
	gen_temp(ctx, &cond, &qbe_word, "cond.%d");
	gen_expression(ctx, expr->_if.cond, &cond);

	struct qbe_statement tlabel = {0}, flabel = {0}, endl = {0};
	struct qbe_value tbranch = {0}, fbranch = {0}, end = {0};
	tbranch.kind = QV_LABEL;
	tbranch.name = strdup(genl(&tlabel, &ctx->id, "branch_true.%d"));
	fbranch.kind = QV_LABEL;
	fbranch.name = strdup(genl(&flabel, &ctx->id, "branch_false.%d"));
	end.name = strdup(genl(&endl, &ctx->id, "end.%d"));
	end.kind = QV_LABEL;

	pushi(ctx->current, NULL, Q_JNZ, &cond, &tbranch, &fbranch, NULL);

	push(&ctx->current->body, &tlabel);
	gen_expression(ctx, expr->_if.true_branch, out);
	if (!expr->_if.true_branch->terminates) {
		pushi(ctx->current, NULL, Q_JMP, &end, NULL);
	}

	push(&ctx->current->body, &flabel);
	if (expr->_if.false_branch) {
		gen_expression(ctx, expr->_if.false_branch, out);
		if (!expr->_if.false_branch->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &end, NULL);
		}
	}

	push(&ctx->current->body, &endl);
}

static void
gen_expr_list(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct expressions *exprs = &expr->list.exprs;
	while (exprs) {
		const struct qbe_value *dest = NULL;
		if (!exprs->next) {
			dest = out; // Last value determines expression result
		}
		gen_expression(ctx, exprs->expr, dest);
		exprs = exprs->next;
	}
}

static void
gen_expr_measure(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value temp = {0}, ptr = {0};
	switch (expr->measure.op) {
	case M_LEN:
		switch (expr->measure.value->result->storage) {
		case TYPE_STORAGE_ARRAY:
			gen_temp(ctx, &temp,
				qtype_for_type(ctx, expr->result, false),
				"len.%d");
			constl(&temp, expr->measure.value->result->array.length);
			gen_store(ctx, out, &temp);
			break;
		case TYPE_STORAGE_SLICE:
		case TYPE_STORAGE_STRING:
			gen_temp(ctx, &ptr, &qbe_long, "ptr.%d");
			qval_address(&ptr);
			gen_expression(ctx, expr->measure.value, &ptr);
			constl(&temp, builtin_type_size.size);
			pushi(ctx->current, &ptr, Q_ADD, &ptr, &temp, NULL);
			qval_deref(&ptr);
			gen_load(ctx, out, &ptr, false);
			break;
		default:
			assert(0); // Invariant
		}
		break;
	case M_SIZE:
		gen_temp(ctx, &temp,
			qtype_for_type(ctx, expr->result, false),
			"size.%d");
		constl(&temp, expr->measure.type->size);
		gen_store(ctx, out, &temp);
		break;
	case M_OFFSET:
		assert(0); // TODO
	}
}

static void
gen_expr_return(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (expr->_return.value) {
		gen_expression(ctx, expr->_return.value, ctx->return_value);
	}
	pushi(ctx->current, NULL, Q_JMP, ctx->end_label, NULL);
}

static void
gen_expr_struct(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	// XXX: ARCH
	struct qbe_value base = {0}, ptr = {0}, offset = {0};
	gen_temp(ctx, &base, &qbe_long, "base.%d");
	gen_temp(ctx, &ptr, &qbe_long, "ptr.%d");
	pushi(ctx->current, &base, Q_COPY, out, NULL);

	const struct expression_struct *field = &expr->_struct;
	while (field) {
		constl(&offset, field->field->offset);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offset, NULL);
		ptr.indirect = !type_is_aggregate(field->field->type);
		gen_expression(ctx, field->value, &ptr);
		field = field->next;
	}
}

static void
gen_expr_address(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct expression *operand = expr->unarithm.operand;
	assert(operand->type == EXPR_ACCESS); // Invariant

	struct qbe_value src = {0};
	address_object(ctx, operand, &src);
	qval_address(&src);
	gen_store(ctx, out, &src);
}

static void
gen_expr_unarithm(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (expr->unarithm.op == UN_ADDRESS) {
		gen_expr_address(ctx, expr, out);
		return;
	}

	struct expression *operand = expr->unarithm.operand;
	struct qbe_value op = {0}, res = {0};
	gen_temp(ctx, &op, qtype_for_type(ctx, operand->result, false), "operand.%d");
	gen_temp(ctx, &res, qtype_for_type(ctx, expr->result, false), "result.%d");
	gen_expression(ctx, expr->unarithm.operand, &op);

	struct qbe_value temp = {0};
	temp.kind = QV_CONST;
	temp.type = op.type;
	switch (expr->unarithm.op) {
	case UN_LNOT:
		temp.lval = 1;
		pushi(ctx->current, &res, Q_XOR, &temp, &op, NULL);
		break;
	case UN_BNOT:
		temp.lval = (uint64_t)-1;
		pushi(ctx->current, &res, Q_XOR, &temp, &op, NULL);
		break;
	case UN_MINUS:
		temp.lval = 0;
		pushi(ctx->current, &res, Q_SUB, &temp, &op, NULL);
		break;
	case UN_PLUS:
		res = op; // no-op
		break;
	case UN_DEREF:
		if (!type_is_aggregate(expr->result)) {
			qval_deref(&op);
		}
		gen_load(ctx, &res, &op, type_is_signed(expr->result));
		break;
	case UN_ADDRESS:
		assert(0); // Invariant
	}

	gen_store(ctx, out, &res);
}

static void
gen_expression(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	switch (expr->type) {
	case EXPR_ACCESS:
		gen_expr_access(ctx, expr, out);
		break;
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
		assert(0); // TODO
	case EXPR_CALL:
		gen_expr_call(ctx, expr, out);
		break;
	case EXPR_CAST:
		gen_expr_cast(ctx, expr, out);
		break;
	case EXPR_CONSTANT:
		gen_expr_constant(ctx, expr, out);
		break;
	case EXPR_CONTINUE:
		assert(0); // TODO
	case EXPR_FOR:
		gen_expr_for(ctx, expr, out);
		break;
	case EXPR_IF:
		gen_expr_if(ctx, expr, out);
		break;
	case EXPR_LIST:
		gen_expr_list(ctx, expr, out);
		break;
	case EXPR_MATCH:
		assert(0); // TODO
	case EXPR_MEASURE:
		gen_expr_measure(ctx, expr, out);
		break;
	case EXPR_RETURN:
		gen_expr_return(ctx, expr, out);
		break;
	case EXPR_SLICE:
		assert(0); // TODO
	case EXPR_STRUCT:
		gen_expr_struct(ctx, expr, out);
		break;
	case EXPR_SWITCH:
		assert(0); // TODO
	case EXPR_UNARITHM:
		gen_expr_unarithm(ctx, expr, out);
		break;
	}
}

static void
gen_function_decl(struct gen_context *ctx, const struct declaration *decl)
{
	// XXX: AUDIT ME
	assert(decl->type == DECL_FUNC);
	const struct function_decl *func = &decl->func;
	const struct type *fntype = func->type;
	assert(func->flags == 0); // TODO

	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_FUNC;
	qdef->exported = decl->exported;
	qdef->name = func->symbol ? strdup(func->symbol)
		: ident_to_sym(&decl->ident);
	qdef->func.returns = qtype_for_type(ctx, fntype->func.result, false);
	ctx->current = &qdef->func;

	struct qbe_statement start_label = {0};
	genl(&start_label, &ctx->id, "start.%d");
	push(&qdef->func.prelude, &start_label);

	struct qbe_func_param *param, **next = &qdef->func.params;
	struct scope_object *obj = decl->func.scope->objects;
	while (obj) {
		param = *next = xcalloc(1, sizeof(struct qbe_func_param));
		assert(!obj->ident.ns); // Invariant
		param->name = strdup(obj->ident.name);
		param->type = qtype_for_type(ctx, obj->type, true);

		if (type_is_aggregate(obj->type)) {
			struct gen_binding *binding =
				xcalloc(1, sizeof(struct gen_binding));
			binding->name = strdup(param->name);
			binding->object = obj;
			binding->next = ctx->bindings;
			ctx->bindings = binding;
		} else {
			struct qbe_value val;
			binding_alloc(ctx, obj, &val, "param.%d");
			struct qbe_value src = {
				.kind = QV_TEMPORARY,
				.type = param->type,
				.name = param->name,
			};
			gen_store(ctx, &val, &src);
		}

		obj = obj->next;
		next = &param->next;
	}

	struct qbe_statement end_label = {0};
	struct qbe_value end_label_v = {
		.kind = QV_LABEL,
		.name = strdup(genl(&end_label, &ctx->id, "end.%d")),
	};
	ctx->end_label = &end_label_v;

	struct qbe_value rval = {0};
	if (fntype->func.result->storage != TYPE_STORAGE_VOID) {
		alloc_temp(ctx, &rval, fntype->func.result, "ret.%d");
		ctx->return_value = &rval;
	} else {
		ctx->return_value = NULL;
	}

	pushl(&qdef->func, &ctx->id, "body.%d");
	gen_expression(ctx, func->body, ctx->return_value);
	push(&qdef->func.body, &end_label);

	if (fntype->func.result->storage != TYPE_STORAGE_VOID) {
		struct qbe_value load = {0};
		gen_loadtemp(ctx, &load, ctx->return_value,
			qdef->func.returns,
			type_is_signed(fntype->func.result));
		pushi(&qdef->func, NULL, Q_RET, &load, NULL);
	} else {
		pushi(&qdef->func, NULL, Q_RET, NULL);
	}

	// Free bindings
	struct gen_binding *binding = ctx->bindings;
	while (binding) {
		struct gen_binding *next = binding->next;
		free(binding->name);
		free(binding);
		binding = next;
	}
	ctx->bindings = NULL;

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
	case DECL_TYPE:
	case DECL_GLOBAL:
	case DECL_CONSTANT:
		assert(0); // TODO
	}
}

void
gen(const struct unit *unit, struct qbe_program *out)
{
	struct gen_context ctx = {
		.out = out,
		.ns = unit->ns,
	};
	ctx.out->next = &ctx.out->defs;
	const struct declarations *decls = unit->declarations;
	assert(decls); // At least one is required
	trenter(TR_GEN, "gen");
	while (decls) {
		gen_decl(&ctx, decls->decl);
		decls = decls->next;
	}
	trleave(TR_GEN, NULL);
}
