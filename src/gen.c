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

static char *
ident_to_sym(const struct identifier *ident)
{
	if (ident->ns) {
		char *ns = ident_to_sym(ident->ns);
		if (!ns) {
			return NULL;
		}
		int n = snprintf(NULL, 0, "%s.%s", ns, ident->name);
		char *str = calloc(1, n + 1);
		assert(str);
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
	val->kind = QV_TEMPORARY;
	val->type = type;

	int n = snprintf(NULL, 0, fmt, ctx->id);
	char *str = calloc(1, n + 1);
	snprintf(str, n + 1, fmt, ctx->id);
	++ctx->id;

	val->name = str;
}

static void
alloc_temp(struct gen_context *ctx, struct qbe_value *val,
		const struct type *type, const char *fmt)
{
	gen_temp(ctx, val, &qbe_long, fmt); // XXX: Architecture dependent
	val->indirect = true;
	val->type = qtype_for_type(ctx, type, false);

	struct qbe_value size;
	constl(&size, type->size);
	pushi(ctx->current, val, alloc_for_align(type->align), &size, NULL);
}

static struct gen_binding *
binding_alloc(struct gen_context *ctx, const struct scope_object *obj,
		struct qbe_value *val, const char *fmt)
{
	alloc_temp(ctx, val, obj->type, fmt);
	val->indirect = true;

	struct gen_binding *binding = calloc(1, sizeof(struct gen_binding));
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
	const struct gen_binding *binding = binding_lookup(ctx, obj);
	val->kind = QV_TEMPORARY; // XXX: Is this always the case?
	val->indirect = true; // XXX: Is this always the case?
	val->type = qtype_for_type(ctx, obj->type, false);
	val->name = binding ? strdup(binding->name) : ident_to_sym(&obj->ident);
}

// Given a pointer temporary, convert it to a dereferenced pointer for the given
// secondary type
static void
qval_deref(struct gen_context *ctx,
	struct qbe_value *val, const struct type *type)
{
	assert(val->type == &qbe_long); // Invariant // XXX: ARCH
	val->indirect = true;
	val->type = qtype_for_type(ctx, type, false);
}

// Given a non-pointer temporary, convert it to a pointer
static void
qval_address(struct qbe_value *val)
{
	val->type = &qbe_long; // XXX: ARCH
	val->indirect = false;
}

// Given value src of type A, and value dest of type pointer to A, store src in
// dest.
static void
gen_store(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src)
{
	if (!dest) {
		// no-op
		return;
	}

	const struct qbe_type *qtype = src->type;
	assert(qtype->stype != Q__VOID); // Invariant
	assert(qtype->stype != Q__AGGREGATE); // TODO

	if (dest->indirect) {
		pushi(ctx->current, NULL, store_for_type(qtype->stype), src, dest, NULL);
	} else {
		pushi(ctx->current, dest, Q_COPY, src, NULL);
	}
}

// Given value src of type pointer to A, and value dest of type A, load dest
// from src.
static void
gen_load(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src,
	bool is_signed)
{
	const struct qbe_type *qtype = dest->type;
	assert(qtype->stype != Q__VOID); // Invariant
	assert(qtype->stype != Q__AGGREGATE); // TODO
	pushi(ctx->current, dest,
		load_for_type(qtype->stype, is_signed), src, NULL);
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
gen_access(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (out == NULL) {
		pushc(ctx->current, "useless access expression discarded");
		return;
	}

	assert(expr->access.type == ACCESS_IDENTIFIER); // TODO
	const struct scope_object *obj = expr->access.object;
	struct qbe_value src;
	qval_for_object(ctx, &src, obj);

	struct qbe_value temp;
	gen_loadtemp(ctx, &temp, &src, src.type, type_is_signed(obj->type));
	gen_store(ctx, out, &temp);
}

static void
gen_assign(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL); // Invariant

	const struct expression *object = expr->assign.object;
	const struct expression *value = expr->assign.value;

	if (expr->assign.indirect) {
		struct qbe_value dest = {0};
		gen_temp(ctx, &dest, &qbe_long, "indirect.%d"); // XXX: ARCH
		gen_expression(ctx, object, &dest);
		qval_deref(ctx, &dest, value->result);
		gen_expression(ctx, value, &dest);
	} else {
		assert(object->type == EXPR_ACCESS); // Invariant
		// TODO: When this grows to support e.g. indexing expressions,
		// we need to ensure that the side-effects of the lvalue occur
		// before the side-effects of the rvalue.
		const struct scope_object *obj = object->access.object;
		struct qbe_value src;
		qval_for_object(ctx, &src, obj);
		gen_expression(ctx, value, &src);
	}
}

static void
gen_binding(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL);

	const struct expression_binding *binding = &expr->binding;
	while (binding) {
		const struct type *type = binding->object->type;
		assert(!type_is_aggregate(type)); // TODO

		struct qbe_value temp;
		binding_alloc(ctx, binding->object, &temp, "binding.%d");
		gen_expression(ctx, binding->initializer, &temp);

		binding = binding->next;
	}
}

static void
gen_binarithm(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct qbe_type *ltype =
		qtype_for_type(ctx, expr->binarithm.lvalue->result, false);
	const struct qbe_type *rtype =
		qtype_for_type(ctx, expr->binarithm.rvalue->result, false);
	const struct qbe_type *etype = qtype_for_type(ctx, expr->result, false);
	assert(etype == ltype && ltype == rtype); // TODO: Type promotion

	assert(expr->result != &builtin_type_bool); // TODO: Logical arithmetic

	struct qbe_value lvalue = {0}, rvalue = {0}, result = {0};
	gen_temp(ctx, &lvalue, ltype, "lvalue.%d");
	gen_temp(ctx, &rvalue, rtype, "rvalue.%d");
	gen_temp(ctx, &result, etype, "result.%d");

	gen_expression(ctx, expr->binarithm.lvalue, &lvalue);
	gen_expression(ctx, expr->binarithm.rvalue, &rvalue);

	pushi(ctx->current, &result, binarithm_for_op(expr->binarithm.op),
			&lvalue, &rvalue, NULL);
	gen_store(ctx, out, &result);
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

	const struct qbe_type *qtype = qtype_for_type(ctx, expr->result, false);
	struct qbe_value val = {0};
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
		assert(0); // TODO: General-purpose store
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
			// Last value determines expression result
			dest = out;
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
gen_expr_unarithm(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct qbe_type *otype =
		qtype_for_type(ctx, expr->unarithm.operand->result, false);
	const struct qbe_type *rtype = qtype_for_type(ctx, expr->result, false);
	if (expr->unarithm.op == UN_ADDRESS) { // Special case
		const struct expression *operand = expr->unarithm.operand;
		assert(operand->type == EXPR_ACCESS); // Invariant
		assert(operand->access.type == ACCESS_IDENTIFIER); // TODO
		const struct scope_object *obj = operand->access.object;

		struct qbe_value src = {0};
		qval_for_object(ctx, &src, obj);
		qval_address(&src);
		gen_store(ctx, out, &src);
		return;
	}

	struct qbe_value operand = {0}, result = {0};
	gen_temp(ctx, &operand, otype, "operand.%d");
	gen_temp(ctx, &result, rtype, "result.%d");

	gen_expression(ctx, expr->unarithm.operand, &operand);

	struct qbe_value temp = {0};
	temp.kind = QV_CONST;
	temp.type = otype;
	switch (expr->unarithm.op) {
	case UN_LNOT:
		temp.lval = 1;
		pushi(ctx->current, &result, Q_XOR, &temp, &operand, NULL);
		break;
	case UN_BNOT:
		temp.lval = (uint64_t)-1;
		pushi(ctx->current, &result, Q_XOR, &temp, &operand, NULL);
		break;
	case UN_MINUS:
		temp.lval = 0;
		pushi(ctx->current, &result, Q_SUB, &temp, &operand, NULL);
		break;
	case UN_PLUS:
		// no-op
		result = operand;
		break;
	case UN_DEREF:
		qval_deref(ctx, &operand, expr->result);
		gen_load(ctx, &result, &operand, type_is_signed(expr->result));
		break;
	case UN_ADDRESS:
		assert(0); // Invariant
	}

	gen_store(ctx, out, &result);
}

static void
gen_expression(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	switch (expr->type) {
	case EXPR_ACCESS:
		gen_access(ctx, expr, out);
		break;
	case EXPR_ASSERT:
		assert(0); // TODO
	case EXPR_ASSIGN:
		gen_assign(ctx, expr, out);
		break;
	case EXPR_BINARITHM:
		gen_binarithm(ctx, expr, out);
		break;
	case EXPR_BINDING:
		gen_binding(ctx, expr, out);
		break;
	case EXPR_BREAK:
	case EXPR_CALL:
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		gen_constant(ctx, expr, out);
		break;
	case EXPR_CONTINUE:
	case EXPR_FOR:
	case EXPR_FREE:
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
	assert(decl->type == DECL_FUNC);
	const struct function_decl *func = &decl->func;
	const struct type *fntype = func->type;
	assert(func->flags == 0); // TODO

	struct qbe_def *qdef = calloc(1, sizeof(struct qbe_def));
	qdef->type = Q_FUNC;
	qdef->exported = decl->exported;
	qdef->name = func->symbol ? strdup(func->symbol)
		: ident_to_sym(&decl->ident);
	qdef->func.returns = qtype_for_type(ctx, fntype->func.result, true);
	ctx->current = &qdef->func;

	pushl(&qdef->func, &ctx->id, "start.%d");

	struct qbe_func_param *param, **next = &qdef->func.params;
	struct scope_object *obj = decl->func.scope->objects;
	while (obj) {
		param = *next = calloc(1, sizeof(struct qbe_func_param));
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
	push(&qdef->func, &end_label);

	if (fntype->func.result->storage != TYPE_STORAGE_VOID) {
		struct qbe_value load;
		gen_loadtemp(ctx, &load, ctx->return_value, qdef->func.returns,
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
