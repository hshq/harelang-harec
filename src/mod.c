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

void
write_ident(FILE *out, const struct identifier *ident)
{
	if (ident->ns) {
		write_ident(out, ident->ns);
		fprintf(out, "::");
	};
	fprintf(out, "%s", ident->name);
}

struct scope *
module_resolve(struct modcache *cache[],
	struct ast_global_decl *defines,
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

	char env[PATH_MAX+1];
	FILE *envf = fmemopen(&env, sizeof(env), "w");
	fprintf(envf, "HARE_TD_");
	write_ident(envf, ident);
	fclose(envf);

	char *path = getenv(env);
	if (!path) {
		fprintf(stderr, "Could not open module '%s': typedef variable $%s not set\n",
			identifier_unparse(ident), env);
		exit(EXIT_FAILURE);
	}

	FILE *f = fopen(path, "r");
	if (!f) {
		fprintf(stderr, "Could not open module '%s' for reading from %s: %s\n",
				identifier_unparse(ident), path,
				strerror(errno));
		exit(EXIT_FAILURE);
	}

	const char *old = sources[0];
	sources[0] = path;
	lex_init(&lexer, f, 0);
	parse(&lexer, &aunit.subunits);
	lex_finish(&lexer);

	// TODO: Free unused bits
	struct unit u = {0};
	struct scope *scope = check_internal(store,
			cache, NULL, defines, &aunit, &u, true);

	sources[0] = old;
	bucket = &cache[hash % MODCACHE_BUCKETS];
	struct modcache *item = xcalloc(1, sizeof(struct modcache));
	identifier_dup(&item->ident, ident);
	item->scope = scope;
	item->next = *bucket;
	*bucket = item;
	return scope;
}
