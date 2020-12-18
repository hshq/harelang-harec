#include <assert.h>
#include <stdlib.h>
#include "ast.h"
#include "check.h"
#include "types.h"
#include "type_store.h"

struct context {
	struct type_store store;
};

static void
check_function(struct context *ctx,
	const struct ast_function_decl *adecl,
	struct declaration *decl)
{
	decl->type = DECL_FUNC;
	assert(0); // TODO
}

static void
check_declarations(struct context *ctx,
		const struct ast_decls *adecls,
		struct declarations **next)
{
	while (adecls) {
		struct declarations *decls = *next =
			calloc(1, sizeof(struct declarations));
		struct declaration *decl = &decls->decl;
		const struct ast_decl *adecl = &adecls->decl;
		decl->exported = adecl->exported;
		switch (adecl->decl_type) {
		case AST_DECL_FUNC:
			check_function(ctx, &adecl->function, decl);
			break;
		case AST_DECL_TYPE:
			assert(0); // TODO
		case AST_DECL_GLOBAL:
			assert(0); // TODO
		case AST_DECL_CONST:
			assert(0); // TODO
		}
		adecls = adecls->next;
		next = &decls->next;
	}
}

static void
scan_function(struct context *ctx, const struct ast_function_decl *decl)
{
	const struct ast_type fn_atype = {
		.storage = TYPE_STORAGE_FUNCTION,
		.flags = TYPE_CONST,
		.func = decl->prototype,
	};
	const struct type *fntype = type_store_lookup_atype(
			&ctx->store, &fn_atype);
	assert(fntype); // TODO: Forward references
}

static void
scan_declarations(struct context *ctx, const struct ast_decls *decls)
{
	while (decls) {
		const struct ast_decl *decl = &decls->decl;
		switch (decl->decl_type) {
		case AST_DECL_FUNC:
			scan_function(ctx, &decl->function);
			break;
		case AST_DECL_TYPE:
			assert(0); // TODO
		case AST_DECL_GLOBAL:
			assert(0); // TODO
		case AST_DECL_CONST:
			assert(0); // TODO
		}
		decls = decls->next;
	}
}

void
check(const struct ast_unit *aunit, struct unit *unit)
{
	struct context ctx = {0};
	// First pass populates the type graph
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		scan_declarations(&ctx, &su->decls);
	}

	// Second pass populates the expression graph
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		check_declarations(&ctx, &su->decls, &unit->declarations);
	}

	assert(unit->declarations);
}
