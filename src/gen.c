#include <assert.h>
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

static struct gen_temp *
alloc_temp(struct gen_context *ctx, const struct type *type, const char *fmt)
{
	assert(type->size != 0 && type->size != SIZE_UNDEFINED);

	struct gen_temp *temp = xcalloc(1, sizeof(struct gen_temp));
	temp->type = type;
	temp->name = gen_name(ctx, fmt);

	// TODO: Look up qbe type
	assert(type_dealias(type)->storage == STORAGE_INT);
	struct qbe_value out = {
		.kind = QV_TEMPORARY,
		.type = &qbe_word,
		.name = temp->name,
	};
	struct qbe_value size;
	constl(&size, type->size);
	pushprei(ctx->current, &out, alloc_for_align(type->align), &size, NULL);

	return temp;
}

static void
gen_expr(struct gen_context *ctx, const struct expression *expr)
{
	switch (expr->type) {
	case EXPR_ACCESS:
	case EXPR_ALLOC:
	case EXPR_APPEND:
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINARITHM:
	case EXPR_BINDING:
	case EXPR_BREAK:
	case EXPR_CONTINUE:
	case EXPR_CALL:
	case EXPR_CAST:
	case EXPR_CONSTANT:
	case EXPR_DEFER:
	case EXPR_DELETE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
	case EXPR_INSERT:
	case EXPR_LIST:
	case EXPR_MATCH:
	case EXPR_MEASURE:
		assert(0); // TODO
	case EXPR_PROPAGATE:
		assert(0); // Lowered in check (XXX: for now...)
	case EXPR_RETURN:
	case EXPR_SLICE:
	case EXPR_STRUCT:
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
	genl(&start_label, &ctx->id, "start.%d");
	push(&qdef->func.prelude, &start_label);

	if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		alloc_temp(ctx, fntype->func.result, "rval.%d");
		// TODO: Look up qbe type
		assert(type_dealias(fntype->func.result)->storage == STORAGE_INT);
		qdef->func.returns = &qbe_word;
	} else {
		qdef->func.returns = &qbe_void;
	}

	// TODO: Allocate parameters
	assert(!func->scope->objects);

	pushl(&qdef->func, &ctx->id, "body.%d");
	gen_expr(ctx, func->body);

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
gen(const struct unit *unit, struct qbe_program *out)
{
	struct gen_context ctx = {
		.out = out,
		.ns = unit->ns,
	};
	ctx.out->next = &ctx.out->defs;
	const struct declarations *decls = unit->declarations;
	while (decls) {
		gen_decl(&ctx, decls->decl);
		decls = decls->next;
	}
}
