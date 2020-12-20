#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "check.h"
#include "emit.h"
#include "gen.h"
#include "lex.h"
#include "parse.h"
#include "qbe.h"

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
	enum stage stage = parse_stage(getenv("HA_STAGE"));

	struct lexer lexer;
	lex_init(&lexer, stdin);
	if (stage == STAGE_LEX) {
		struct token tok;
		while (lex(&lexer, &tok) != T_EOF);
		lex_finish(&lexer);
		return 0;
	}

	struct ast_unit aunit = {0};
	parse(&lexer, &aunit.subunits);
	lex_finish(&lexer);
	if (stage == STAGE_PARSE) {
		return 0;
	}

	struct unit unit = {0};
	check(&aunit, &unit);
	if (stage == STAGE_CHECK) {
		return 0;
	}

	struct qbe_program prog = {0};
	gen(&unit, &prog);
	if (stage == STAGE_GEN) {
		return 0;
	}

	emit(&prog, stdout);
	return 0;
}
