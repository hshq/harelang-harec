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

	assert(type_dealias(fntype->func.result)->storage == STORAGE_VOID);
	// TODO: non-void return type
	qdef->func.returns = &qbe_void;

	assert(!decl->func.scope->objects); // TODO: Parameters

	pushl(&qdef->func, &ctx->id, "body.%d");
	// TODO: Generate body

	push(&qdef->func.body, &end_label);
	// TODO: non-void return value
	pushi(ctx->current, NULL, Q_RET, NULL);

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
