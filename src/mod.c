#include <assert.h>
#include <errno.h>
#include <limits.h>
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
module_resolve(struct identifier *ident, struct type_store *store)
{
	struct lexer lexer = {0};
	struct ast_unit aunit = {0};

	const char *path = open_typedefs(ident);
	FILE *f = fopen(path, "r");
	if (!f) {
		fprintf(stderr, "Could not open module '%s' for reading: %s\n",
				identifier_unparse(ident),
				strerror(errno));
		exit(1);
	}

	lex_init(&lexer, f, path);
	parse(&lexer, &aunit.subunits);
	lex_finish(&lexer);

	// TODO: Free unused bits
	struct unit u = {0};
	return check(store, &aunit, &u);
}
