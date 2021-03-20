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

static char *
ident_to_env(const struct identifier *ident)
{
	if (ident->ns) {
		char *ns = ident_to_env(ident->ns);
		if (!ns) {
			return NULL;
		}
		int n = snprintf(NULL, 0, "%s_%s", ns, ident->name);
		char *str = xcalloc(1, n + 1);
		snprintf(str, n + 1, "%s_%s", ns, ident->name);
		free(ns);
		return str;
	}
	return strdup(ident->name);
}


static const char *
open_typedefs(struct identifier *ident)
{
	char *env = ident_to_env(ident);
	char versenv[PATH_MAX+1];
	snprintf(versenv, sizeof(versenv), "HARE_VERSION_%s", env);

	char *version = getenv(versenv);
	if (!version) {
		version = env;
	} else {
		version = strdup(version);
		free(env);
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
	struct define *defines,
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
			cache, NULL, defines, &aunit, &u, true);

	bucket = &cache[hash % MODCACHE_BUCKETS];
	struct modcache *item = xcalloc(1, sizeof(struct modcache));
	identifier_dup(&item->ident, ident);
	item->scope = scope;
	item->next = *bucket;
	*bucket = item;
	return scope;
}
