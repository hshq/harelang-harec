#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "ast.h"
#include "check.h"
#include "dump.h"
#include "emit.h"
#include "gen.h"
#include "lex.h"
#include "parse.h"
#include "qbe.h"
#include "typedef.h"
#include "type_store.h"
#include "util.h"

static void
usage(const char *argv_0)
{
	fprintf(stderr,
		"Usage: %s [-o output] [-T tags...] [-t typdefs] [-N namespace]\n",
		argv_0);
}

enum stage {
	STAGE_LEX,
	STAGE_PARSE,
	STAGE_CHECK,
	STAGE_GEN,
	STAGE_EMIT,
};

enum stage
parse_stage(const char *s)
{
	if (s == NULL) {
		return STAGE_EMIT;
	} else if (strcmp(s, "lex") == 0) {
		return STAGE_LEX;
	} else if (strcmp(s, "parse") == 0) {
		return STAGE_PARSE;
	} else if (strcmp(s, "check") == 0) {
		return STAGE_CHECK;
	} else if (strcmp(s, "gen") == 0) {
		return STAGE_GEN;
	} else if (strcmp(s, "emit") == 0) {
		return STAGE_EMIT;
	} else {
		fprintf(stderr, "Unknown HA_STAGE value '%s'\n", s);
		exit(1);
	}
}

int
main(int argc, char *argv[])
{
	char *output = NULL, *typedefs = NULL;
	struct unit unit = {0};
	struct lexer lexer;

	int c;
	while ((c = getopt(argc, argv, "o:T:t:N:")) != -1) {
		switch (c) {
		case 'o':
			output = optarg;
			break;
		case 'T':
			assert(0); // TODO: Build tags
		case 't':
			typedefs = optarg;
			break;
		case 'N':
			unit.ns = xcalloc(1, sizeof(struct identifier));
			FILE *in = fmemopen(optarg, strlen(optarg), "r");
			lex_init(&lexer, in, "-N");
			parse_identifier(&lexer, unit.ns);
			lex_finish(&lexer);
			break;
		default:
			usage(argv[0]);
			return 1;
		}
	}

	size_t ninputs = argc - optind;
	if (ninputs == 0) {
		usage(argv[0]);
		return 1;
	}

	struct ast_unit aunit = {0};
	struct ast_subunit *subunit = &aunit.subunits;
	struct ast_subunit **next = &aunit.subunits.next;
	enum stage stage = parse_stage(getenv("HA_STAGE"));

	for (size_t i = 0; i < ninputs; ++i) {
		FILE *in;
		const char *path = argv[optind + i];
		if (strcmp(path, "-") == 0) {
			in = stdin;
		} else {
			in = fopen(path, "r");
		}

		if (!in) {
			fprintf(stderr, "Unable to open %s for reading: %s\n",
					path, strerror(errno));
			return 1;
		}

		lex_init(&lexer, in, path);
		if (stage == STAGE_LEX) {
			struct token tok;
			while (lex(&lexer, &tok) != T_EOF);
		} else {
			parse(&lexer, subunit);
			if (i + 1 < ninputs) {
				*next = xcalloc(1, sizeof(struct ast_subunit));
				subunit = *next;
				next = &subunit->next;
			}
		}
		lex_finish(&lexer);
	}

	if (stage == STAGE_PARSE || stage == STAGE_LEX) {
		return 0;
	}

	struct type_store ts = {0};
	builtin_types_init();
	check(&ts, &aunit, &unit);
	if (stage == STAGE_CHECK) {
		dump_unit(&unit);
		return 0;
	}

	if (typedefs) {
		FILE *out = fopen(typedefs, "w");
		if (!out) {
			fprintf(stderr, "Unable to open %s for writing: %s\n",
					typedefs, strerror(errno));
			return 1;
		}
		emit_typedefs(&unit, out);
		fclose(out);
	}

	struct qbe_program prog = {0};
	gen(&unit, &prog);
	if (stage == STAGE_GEN) {
		return 0;
	}

	FILE *out;
	if (!output) {
		out = stdout;
	} else {
		out = fopen(output, "w");
		if (!out) {
			fprintf(stderr, "Unable to open %s for writing: %s\n",
					output, strerror(errno));
			return 1;
		}
	}
	emit(&prog, out);
	fclose(out);
	return 0;
}
