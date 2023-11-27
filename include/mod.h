#ifndef HARE_MOD_H
#define HARE_MOD_H
#include "identifier.h"
#include "scope.h"

struct ast_global_decl;
struct context;
struct scope *module_resolve(struct context *ctx,
	const struct ast_global_decl *defines,
	const struct identifier *ident);

#endif
