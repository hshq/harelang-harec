#include <stdio.h>
#include "lex.h"

int
main(int argc, char *argv[])
{
	struct token tok;
	struct lexer lexer;
	lex_init(&lexer, stdin);
	while (lex(&lexer, &tok) != EOF) {
		// TODO
	};
	return 0;
}
