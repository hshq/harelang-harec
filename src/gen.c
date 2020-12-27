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
	pushprei(ctx->current, val, alloc_for_align(type->align), &size, NULL);
}

static struct gen_binding *
binding_alloc(struct gen_context *ctx, const struct scope_object *obj,
		struct qbe_value *val, const char *fmt)
{
	struct gen_binding *binding = xcalloc(1, sizeof(struct gen_binding));
	alloc_temp(ctx, val, obj->type, fmt);
	binding->name = strdup(val->name);
	binding->object = obj;
	binding->next = ctx->bindings;
	ctx->bindings = binding;
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
		break;
	case O_DECL:
		val->kind = QV_GLOBAL;
		val->indirect = false;
		break;
	}

	if (type_is_aggregate(obj->type)) {
		val->type = &qbe_aggregate;
	} else {
		val->type = &qbe_long; // XXX: ARCH
	}

	val->name = binding ? strdup(binding->name) : ident_to_sym(&obj->ident);
}

static void
qval_deref(struct qbe_value *val)
{
	assert(val->type == &qbe_long); // Invariant // XXX: ARCH
	val->indirect = true;
}

static void
qval_address(struct qbe_value *val)
{
	assert(val->type == &qbe_long); // XXX: ARCH
	val->indirect = false;
}

static void
gen_store(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src)
{
	assert(src && !src->indirect); // Invariant
	if (!dest) {
		// no-op
		return;
	}

	const struct qbe_type *qtype = dest->type;
	assert(qtype->stype != Q__VOID); // Invariant
	assert(qtype->stype != Q__AGGREGATE); // TODO

	if (dest->indirect) {
		pushi(ctx->current, NULL, store_for_type(qtype->stype), src, dest, NULL);
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
	const struct qbe_type *qtype = dest->type;
	assert(qtype->stype != Q__VOID); // Invariant

	if (src->type->stype == Q__AGGREGATE) {
		assert(src->indirect && !dest->indirect);
		pushi(ctx->current, dest, Q_COPY, src, NULL);
	} else if (src->indirect) {
		pushi(ctx->current, dest, load_for_type(
			qtype->stype, is_signed), src, NULL);
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
gen_expr_access_ident(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct scope_object *obj = expr->access.object;

	struct qbe_value src = {0}, temp = {0};
	qval_for_object(ctx, &src, obj);
	if (src.indirect) {
		gen_loadtemp(ctx, &temp, &src, src.type,
				type_is_signed(obj->type));
		gen_store(ctx, out, &temp);
	} else {
		gen_store(ctx, out, &src);
	}
}

static void
gen_expr_access_index(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct type *atype = expr->access.array->result;
	assert(atype->storage == TYPE_STORAGE_ARRAY); // TODO: Slices

	struct qbe_value obj = {0};
	gen_temp(ctx, &obj, &qbe_long, "object.%d"); // XXX: ARCH
	gen_expression(ctx, expr->access.array, &obj);

	struct qbe_value index = {0};
	gen_temp(ctx, &index, &qbe_long, "index.%d");
	gen_expression(ctx, expr->access.index, &index);

	// TODO: Check if offset is in bounds

	struct qbe_value temp = {0};
	constl(&temp, atype->array.members->size);
	pushi(ctx->current, &index, Q_MUL, &index, &temp, NULL);
	pushi(ctx->current, &obj, Q_ADD, &obj, &index, NULL);
	qval_deref(&obj);

	gen_loadtemp(ctx, &temp, &obj,
		qtype_for_type(ctx, atype->array.members, true),
		type_is_signed(atype->array.members));
	gen_store(ctx, out, &temp);
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

	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		gen_expr_access_ident(ctx, expr, out);
		break;
	case ACCESS_INDEX:
		gen_expr_access_index(ctx, expr, out);
		break;
	case ACCESS_FIELD:
		assert(0); // TODO
	}
}

static void
gen_expr_assign_ptr(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct expression *object = expr->assign.object;
	const struct expression *value = expr->assign.value;

	struct qbe_value v = {0};
	gen_temp(ctx, &v, &qbe_long, "indirect.%d"); // XXX: ARCH
	gen_expression(ctx, object, &v);
	qval_deref(&v);
	gen_expression(ctx, value, &v);
}

static void
gen_expr_assign(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL); // Invariant
	if (expr->assign.indirect) {
		gen_expr_assign_ptr(ctx, expr, out);
		return;
	}

	// TODO: When this grows to support e.g. indexing expressions, we need
	// to ensure that the side-effects of the lvalue occur before the
	// side-effects of the rvalue.

	const struct expression *object = expr->assign.object;
	assert(object->type == EXPR_ACCESS); // Invariant
	const struct scope_object *obj = object->access.object;
	const struct expression *value = expr->assign.value;

	struct qbe_value src;
	qval_for_object(ctx, &src, obj);
	gen_expression(ctx, value, &src);
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
gen_expr_binarithm(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct qbe_type *ltype =
		qtype_for_type(ctx, expr->binarithm.lvalue->result, false);
	const struct qbe_type *rtype =
		qtype_for_type(ctx, expr->binarithm.rvalue->result, false);
	const struct qbe_type *etype = qtype_for_type(ctx, expr->result, false);
	assert(etype == ltype && ltype == rtype); // TODO: Type promotion

	struct qbe_value lvalue = {0}, rvalue = {0}, result = {0};
	gen_temp(ctx, &lvalue, ltype, "lvalue.%d");
	gen_temp(ctx, &rvalue, rtype, "rvalue.%d");
	gen_temp(ctx, &result, etype, "result.%d");

	gen_expression(ctx, expr->binarithm.lvalue, &lvalue);
	gen_expression(ctx, expr->binarithm.rvalue, &rvalue);

	pushi(ctx->current, &result,
		binarithm_for_op(expr->binarithm.op, ltype,
			type_is_signed(expr->binarithm.lvalue->result)),
		&lvalue, &rvalue, NULL);
	gen_store(ctx, out, &result);
}

static void
gen_call(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	// XXX: AUDIT ME
	struct qbe_statement call = {
		.type = Q_INSTR,
		.instr = Q_CALL,
		.out = out ? qval_dup(out) : NULL,
	};

	struct qbe_arguments *arg, **next = &call.args;
	struct call_argument *carg = expr->call.args;
	arg = *next = xcalloc(1, sizeof(struct qbe_arguments));
	gen_temp(ctx, &arg->value, &qbe_long, "func.%d");
	gen_expression(ctx, expr->call.lvalue, &arg->value);
	next = &arg->next;

	while (carg) {
		assert(!carg->variadic); // TODO
		arg = *next = xcalloc(1, sizeof(struct qbe_arguments));
		gen_temp(ctx, &arg->value,
			qtype_for_type(ctx, carg->value->result, false),
			"arg.%d");
		gen_expression(ctx, carg->value, &arg->value);
		carg = carg->next;
		next = &arg->next;
	}

	push(&ctx->current->body, &call);
}

static void
gen_array(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct type *type = expr->result;
	assert(out->indirect);			// Invariant
	assert(!type->array.expandable);	// Invariant

	// XXX: ARCH
	struct qbe_value ptr = {0};
	gen_temp(ctx, &ptr, &qbe_long, "ptr.%d");
	pushi(ctx->current, &ptr, Q_COPY, out, NULL);
	ptr.indirect = true;

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
gen_constant(struct gen_context *ctx,
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
	struct qbe_value temp = {0};
	switch (expr->measure.op) {
	case M_LEN:
		assert(0); // TODO
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
gen_expr_address(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct expression *operand = expr->unarithm.operand;
	assert(operand->type == EXPR_ACCESS); // Invariant

	struct qbe_value src = {0};
	switch (operand->access.type) {
	case ACCESS_IDENTIFIER:
		qval_for_object(ctx, &src, operand->access.object);
		break;
	case ACCESS_INDEX:
		assert(0); // TODO
	case ACCESS_FIELD:
		assert(0); // TODO
	}

	assert(src.indirect);
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
		qval_deref(&op);
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
		assert(0); // TODO
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
		gen_call(ctx, expr, out);
		break;
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		gen_constant(ctx, expr, out);
		break;
	case EXPR_CONTINUE:
	case EXPR_FOR:
	case EXPR_IF:
		assert(0); // TODO
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
	case EXPR_STRUCT:
	case EXPR_SWITCH:
		assert(0); // TODO
	case EXPR_UNARITHM:
		gen_expr_unarithm(ctx, expr, out);
		break;
	case EXPR_WHILE:
		assert(0); // TODO
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
	qdef->type = Q_FUNC;
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
			assert(0); // TODO
		} else {
			struct qbe_value val;
			binding_alloc(ctx, obj, &val, "param.%d");
			struct qbe_value src = {
				.kind = QV_TEMPORARY,
				.type = param->type,
				.name = param->name,
			};
			gen_store(ctx, &val, &src);
			free(obj->ident.name);
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
	const struct declarations *decls = unit->declarations;
	assert(decls); // At least one is required
	trenter(TR_GEN, "gen");
	while (decls) {
		gen_decl(&ctx, decls->decl);
		decls = decls->next;
	}
	trleave(TR_GEN, NULL);
}
