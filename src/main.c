#include <stdio.h>
#include "ast.h"
#include "lex.h"
#include "parse.h"

int
main(int argc, char *argv[])
{
	struct lexer lexer;
	lex_init(&lexer, stdin);

	struct ast_unit unit;
	parse(&lexer, NULL, &unit);

	lex_finish(&lexer);
	return 0;
}
