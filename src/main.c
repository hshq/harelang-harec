#include <stdio.h>
#include "lex.h"

int
main(int argc, char *argv[])
{
	struct lexer lexer;
	lex_init(&lexer, stdin);

	struct token tok;
	while (tok.token != T_EOF) {
		lex(&lexer, &tok);
		switch (tok.token) {
		case T_NAME:
			fprintf(stderr, "'%s'\n", tok.name);
			break;
		case T_ERROR:
			fprintf(stderr, "ERROR\n");
			break;
		case T_EOF:
			fprintf(stderr, "EOF\n");
			break;
		default:
			fprintf(stderr, "%s\n", token_str(&tok));
			break;
		}
		token_finish(&tok);
	};

	lex_finish(&lexer);
	return 0;
}
