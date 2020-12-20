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
		const struct qbe_type *type, char *fmt)
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
		const struct type *type, char *fmt)
{
	gen_temp(ctx, val, &qbe_long, fmt); // XXX: Architecture dependent

	struct qbe_value size;
	constl(&size, type->size);
	pushi(ctx->current, alloc_for_align(type->align), val, &size, NULL);
}

static void
gen_constant(struct gen_context *ctx,
	struct qbe_func *body,
	const struct expression *expr,
	struct qbe_value *out)
{
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

	pushi(ctx->current, store_for_type(qtype->stype), NULL, &val, out, NULL);
}

static void
gen_expression(struct gen_context *ctx,
	struct qbe_func *body,
	const struct expression *expr,
	struct qbe_value *out)
{
	switch (expr->type) {
	case EXPR_ACCESS:
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINARITHM:
	case EXPR_BINDING_LIST:
	case EXPR_CALL:
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		gen_constant(ctx, body, expr, out);
		break;
	case EXPR_CONTROL:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_FUNC:
	case EXPR_IF:
	case EXPR_INDEX:
	case EXPR_LIST:
	case EXPR_MATCH:
	case EXPR_MEASURE:
	case EXPR_SLICE:
	case EXPR_STRUCT:
	case EXPR_SWITCH:
	case EXPR_UNARITHM:
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

	assert(fntype->func.params == NULL); // TODO

	pushl(&qdef->func, &ctx->id, "start.%d");

	// TODO: Update for void type
	struct qbe_value rval;
	alloc_temp(ctx, &rval, fntype->func.result, "return.%d");

	// XXX: Does this change if we have an expression list (with an explicit
	// return statement) here?
	pushl(&qdef->func, &ctx->id, "body.%d");
	gen_expression(ctx, &qdef->func, func->body, &rval);
	pushl(&qdef->func, &ctx->id, "end.%d");

	struct qbe_value load;
	gen_temp(ctx, &load, qdef->func.returns, "return.%d");
	pushi(&qdef->func,
		// TODO: Update for aggregate types
		load_for_type(qdef->func.returns->stype, type_is_signed(fntype->func.result)),
		&load, &rval, NULL);
	pushi(&qdef->func, Q_RET, NULL, &load, NULL);

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
		gen_decl(&ctx, &decls->decl);
		decls = decls->next;
	}
	trleave(TR_GEN, NULL);
}
