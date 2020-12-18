#include <assert.h>
#include "ast.h"
#include "check.h"
#include "types.h"
#include "type_store.h"

struct context {
	struct type_store store;
};

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
	const struct ast_subunit *su = &aunit->subunits;
	assert(su); // At least one is required

	// First pass populates the type graph
	while (su) {
		scan_declarations(&ctx, &su->decls);
		su = su->next;
	}

	// Second pass populates the expression graph
	// TODO
}
