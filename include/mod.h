#ifndef HARE_MOD_H
#define HARE_MOD_H
#include "identifier.h"
#include "scope.h"
#include "type_store.h"

struct modcache;
struct scope *module_resolve(struct modcache *cache[],
		struct identifier *ident, struct type_store *store);

#endif
