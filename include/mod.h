#ifndef HARE_MOD_H
#define HARE_MOD_H
#include "identifier.h"
#include "scope.h"
#include "type_store.h"

struct modcache;
struct ast_global_decl;
struct context;
struct scope *module_resolve(struct context *ctx,
	struct ast_global_decl *defines,
	struct identifier *ident,
	struct type_store *store);

#endif
