#include <stdio.h>
#include "ast.h"
#include "check.h"
#include "lex.h"
#include "parse.h"

int
main(int argc, char *argv[])
{
	struct lexer lexer;
	lex_init(&lexer, stdin);

	struct ast_unit aunit = {0};
	parse(&lexer, &aunit.subunits);
	lex_finish(&lexer);

	struct unit unit = {0};
	check(&aunit, &unit);
	return 0;
}
