#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "scope.h"
#include "types.h"
#include "util.h"

static const struct gen_value gv_void = {
	.kind = GV_CONST,
	.type = &builtin_type_void,
};

static void
gen_store(struct gen_context *ctx,
	struct gen_value object,
	struct gen_value value)
{
	switch (type_dealias(object.type)->storage) {
	case STORAGE_ARRAY:
	case STORAGE_ENUM:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
		assert(0); // TODO
	default:
		break; // no-op
	}

	struct qbe_value qobj = mkqval(ctx, &object),
		qval = mkqval(ctx, &value);
	enum qbe_instr qi = store_for_type(ctx, object.type);
	pushi(ctx->current, NULL, qi, &qval, &qobj, NULL);
}

static struct gen_value gen_expr(struct gen_context *ctx,
		const struct expression *expr);

static struct gen_value
gen_expr_binding(struct gen_context *ctx, const struct expression *expr)
{
	for (const struct expression_binding *binding = &expr->binding;
			binding; binding = binding->next) {
		const struct type *type = binding->initializer->result;
		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value.kind = GV_TEMP;
		gb->value.type = type;
		gb->value.name = gen_name(ctx, "binding.%d");
		gb->next = ctx->bindings;
		ctx->bindings = gb;

		struct qbe_value qv = mklval(ctx, &gb->value);
		struct qbe_value sz = constl(type->size);
		enum qbe_instr qi = alloc_for_align(type->align);
		pushprei(ctx->current, &qv, qi, &sz, NULL);

		struct gen_value init = gen_expr(ctx, binding->initializer);
		gen_store(ctx, gb->value, init);
	}
	return gv_void;
}

static struct gen_value
gen_expr_const(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value val = {
		.kind = GV_CONST,
		.type = expr->result,
	};

	// Special cases
	switch (type_dealias(expr->result)->storage) {
	case STORAGE_BOOL:
		val.wval = expr->constant.bval ? 1 : 0;
		return val;
	case STORAGE_VOID:
		return val;
	case STORAGE_NULL:
		val.lval = 0;
		return val;
	case STORAGE_ARRAY:
		assert(0); // TODO
	case STORAGE_STRING:
		assert(0); // TODO
	default:
		// Moving right along
		break;
	}

	const struct qbe_type *qtype = qtype_lookup(ctx, expr->result, false);
	switch (qtype->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
		val.wval = (uint32_t)expr->constant.uval;
		return val;
	case Q_LONG:
		val.lval = expr->constant.uval;
		return val;
	case Q_SINGLE:
		val.sval = (float)expr->constant.fval;
		return val;
	case Q_DOUBLE:
		val.dval = expr->constant.fval;
		return val;
	case Q__VOID:
		return val;
	case Q__AGGREGATE:
		assert(0); // Invariant
	}

	abort(); // Invariant
}

static struct gen_value
gen_expr_list(struct gen_context *ctx, const struct expression *expr)
{
	// TODO: Set up defer scope
	for (const struct expressions *exprs = &expr->list.exprs;
			true; exprs = exprs->next) {
		if (!exprs->next) {
			return gen_expr(ctx, exprs->expr);
		}
		gen_expr(ctx, exprs->expr);
	}
	abort(); // Unreachable
}

static struct gen_value
gen_expr_return(struct gen_context *ctx, const struct expression *expr)
{
	// TODO: Run defers
	struct gen_value ret = gen_expr(ctx, expr->_return.value);
	struct qbe_value qret = mkqval(ctx, &ret);
	pushi(ctx->current, NULL, Q_RET, &qret, NULL);
	return gv_void;
}

static struct gen_value
gen_expr(struct gen_context *ctx, const struct expression *expr)
{
	switch (expr->type) {
	case EXPR_ACCESS:
	case EXPR_ALLOC:
	case EXPR_APPEND:
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINARITHM:
		assert(0); // TODO
	case EXPR_BINDING:
		return gen_expr_binding(ctx, expr);
	case EXPR_BREAK:
	case EXPR_CALL:
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		return gen_expr_const(ctx, expr);
	case EXPR_CONTINUE:
	case EXPR_DEFER:
	case EXPR_DELETE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
	case EXPR_INSERT:
		assert(0); // TODO
	case EXPR_LIST:
		return gen_expr_list(ctx, expr);
	case EXPR_MATCH:
	case EXPR_MEASURE:
		assert(0); // TODO
	case EXPR_PROPAGATE:
		assert(0); // Lowered in check (for now?)
	case EXPR_RETURN:
		return gen_expr_return(ctx, expr);
	case EXPR_SLICE:
	case EXPR_STRUCT:
	case EXPR_SWITCH:
	case EXPR_TUPLE:
	case EXPR_UNARITHM:
		assert(0); // TODO
	}
	abort(); // Unreachable
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
	genl(&start_label, &ctx->id, "start.%d");
	push(&qdef->func.prelude, &start_label);

	if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		qdef->func.returns = qtype_lookup(
			ctx, fntype->func.result, false);
	} else {
		qdef->func.returns = &qbe_void;
	}

	assert(!decl->func.scope->objects); // TODO: Parameters

	pushl(&qdef->func, &ctx->id, "body.%d");
	struct gen_value ret = gen_expr(ctx, decl->func.body);

	if (decl->func.body->terminates) {
		// Do nothing
	} else if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		struct qbe_value qret = mkqval(ctx, &ret);
		pushi(ctx->current, NULL, Q_RET, &qret, NULL);
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
