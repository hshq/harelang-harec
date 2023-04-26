#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include "ast.h"
#include "check.h"
#include "emit.h"
#include "gen.h"
#include "lex.h"
#include "parse.h"
#include "qbe.h"
#include "type_store.h"
#include "typedef.h"
#include "util.h"

static void
usage(const char *argv_0)
{
	fprintf(stderr,
		"Usage: %s [-a arch] [-D ident[:type]=value] [-o output] [-T] [-t typedefs] [-N namespace] input.ha...\n",
		argv_0);
}

enum stage {
	STAGE_LEX,
	STAGE_PARSE,
	STAGE_CHECK,
	STAGE_GEN,
	STAGE_EMIT,
};

static enum stage
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
		exit(EXIT_FAILURE);
	}
}

static struct ast_global_decl *
parse_define(const char *argv_0, const char *in)
{
	struct ast_global_decl *def = xcalloc(1, sizeof(struct ast_global_decl));

	struct token tok;
	struct lexer lexer;
	FILE *f = fmemopen((char *)in, strlen(in), "r");
	const char *d = "-D";
	sources = &d;
	lex_init(&lexer, f, 0);

	parse_identifier(&lexer, &def->ident, false);
	def->type = NULL;
	if (lex(&lexer, &tok) == T_COLON) {
		def->type = parse_type(&lexer);
		lex(&lexer, &tok);
	}

	if (tok.token != T_EQUAL) {
		lex_finish(&lexer);
		usage(argv_0);
		exit(EXIT_FAILURE);
	}
	def->init = parse_expression(&lexer);

	lex_finish(&lexer);
	return def;
}

int
main(int argc, char *argv[])
{
	char *output = NULL, *typedefs = NULL;
	char *target = DEFAULT_TARGET;
	bool is_test = false;
	struct unit unit = {0};
	struct lexer lexer;
	struct ast_global_decl *defines = NULL, **next_def = &defines;

	int c;
	while ((c = getopt(argc, argv, "a:D:ho:Tt:N:")) != -1) {
		switch (c) {
		case 'a':
			target = optarg;
			break;
		case 'D':
			*next_def = parse_define(argv[0], optarg);
			next_def = &(*next_def)->next;
			break;
		case 'o':
			output = optarg;
			break;
		case 'T':
			is_test = true;
			break;
		case 't':
			typedefs = optarg;
			break;
		case 'N':
			unit.ns = xcalloc(1, sizeof(struct identifier));
			FILE *in = fmemopen(optarg, strlen(optarg), "r");
			const char *ns = "-N";
			sources = &ns;
			lex_init(&lexer, in, 0);
			parse_identifier(&lexer, unit.ns, false);
			lex_finish(&lexer);
			break;
		case 'h':
		default:
			usage(argv[0]);
			return EXIT_FAILURE;
		}
	}

	builtin_types_init(target);

	size_t ninputs = argc - optind;
	if (ninputs == 0) {
		usage(argv[0]);
		return EXIT_FAILURE;
	}

	struct ast_unit aunit = {0};
	struct ast_subunit *subunit = &aunit.subunits;
	struct ast_subunit **next = &aunit.subunits.next;
	enum stage stage = parse_stage(getenv("HA_STAGE"));

	sources = xcalloc(ninputs + 2, sizeof(char **));
	memcpy((char **)sources + 1, argv + optind, sizeof(char **) * ninputs);
	sources[0] = "<unknown>";
	sources[ninputs + 1] = NULL;

	for (size_t i = 0; i < ninputs; ++i) {
		FILE *in;
		const char *path = argv[optind + i];
		if (strcmp(path, "-") == 0) {
			in = stdin;
		} else {
			in = fopen(path, "r");
			struct stat buf;
			if (in && fstat(fileno(in), &buf) == 0
				&& S_ISDIR(buf.st_mode) != 0) {
				fprintf(stderr, "Unable to open %s for reading: Is a directory\n",
					path);
				return EXIT_FAILURE;
			}
		}

		if (!in) {
			fprintf(stderr, "Unable to open %s for reading: %s\n",
					path, strerror(errno));
			return EXIT_FAILURE;
		}

		lex_init(&lexer, in,  i + 1);
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
		return EXIT_SUCCESS;
	}

	static struct type_store ts = {0};
	check(&ts, is_test, defines, &aunit, &unit);
	if (stage == STAGE_CHECK) {
		return EXIT_SUCCESS;
	}

	if (typedefs) {
		FILE *out = fopen(typedefs, "w");
		if (!out) {
			fprintf(stderr, "Unable to open %s for writing: %s\n",
					typedefs, strerror(errno));
			return EXIT_FAILURE;
		}
		emit_typedefs(&unit, out);
		fclose(out);
	}

	struct qbe_program prog = {0};
	gen(&unit, &ts, &prog);
	if (stage == STAGE_GEN) {
		return EXIT_SUCCESS;
	}

	FILE *out;
	if (!output) {
		out = stdout;
	} else {
		out = fopen(output, "w");
		if (!out) {
			fprintf(stderr, "Unable to open %s for writing: %s\n",
					output, strerror(errno));
			return EXIT_FAILURE;
		}
	}
	emit(&prog, out);
	fclose(out);
	return EXIT_SUCCESS;
}
