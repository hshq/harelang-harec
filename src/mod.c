#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "identifier.h"
#include "lex.h"
#include "mod.h"
#include "parse.h"
#include "scope.h"
#include "type_store.h"
#include "util.h"

static const char *
open_typedefs(struct identifier *ident)
{
	char *sym = ident_to_sym(ident);
	char versenv[PATH_MAX+1];
	snprintf(versenv, sizeof(versenv), "HARE.%s.VERSION", sym);

	char *version = getenv(versenv);
	if (!version) {
		version = sym;
	} else {
		version = strdup(version);
		free(sym);
	}

	const struct pathspec paths[] = {
		{.var = "HARECACHE", .path = "/%s/%s.td"},
		{.var = "XDG_CACHE_HOME", .path = "/hare/%s/%s.td"},
		{.var = "HOME", .path = "/.cache/hare/%s/%s.td"}
	};
	char *pathfmt = getpath(paths, sizeof(paths) / sizeof(paths[0]));
	assert(pathfmt);

	static char path[PATH_MAX+1];
	const char *ipath = ident_to_path(ident);
	snprintf(path, sizeof(path), pathfmt, ipath, version);
	free(version);
	return path;
}

struct scope *
module_resolve(struct modcache *cache[],
		struct identifier *ident,
		struct type_store *store)
{
	uint32_t hash = identifier_hash(FNV1A_INIT, ident);
	struct modcache **bucket = &cache[hash % MODCACHE_BUCKETS];
	for (; *bucket; bucket = &(*bucket)->next) {
		if (identifier_eq(&(*bucket)->ident, ident)) {
			return (*bucket)->scope;
		}
	}

	struct lexer lexer = {0};
	struct ast_unit aunit = {0};

	const char *path = open_typedefs(ident);
	FILE *f = fopen(path, "r");
	if (!f) {
		fprintf(stderr, "Could not open module '%s' for reading from %s: %s\n",
				identifier_unparse(ident), path,
				strerror(errno));
		exit(1);
	}

	lex_init(&lexer, f, path);
	parse(&lexer, &aunit.subunits);
	lex_finish(&lexer);

	// TODO: Free unused bits
	struct unit u = {0};
	struct scope *scope = check_internal(store,
			cache, NULL, &aunit, &u, true);

	bucket = &cache[hash % MODCACHE_BUCKETS];
	struct modcache *item = xcalloc(1, sizeof(struct modcache));
	identifier_dup(&item->ident, ident);
	item->scope = scope;
	item->next = *bucket;
	*bucket = item;
	return scope;
}
