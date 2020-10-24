#include <stdio.h>
#include "lex.h"
#include "utf8.h"

int
main(int argc, char *argv[])
{
	struct lexer lexer;
	lex_init(&lexer, stdin);

	struct token tok;
	while (tok.token != T_EOF) {
		token_finish(&tok);
		lex(&lexer, &tok);
		switch (tok.token) {
		case T_NAME:
			fprintf(stderr, "'%s'\n", tok.name);
			break;
		case T_LITERAL:
			fprintf(stderr, "(%s)\n", tok.literal);
			break;
		case T_RUNE:
			putc('\'', stderr);
			utf8_fputch(stderr, tok.rune);
			putc('\'', stderr);
			putc('\n', stderr);
			break;
		case T_STRING:
			fprintf(stderr, "\"%*s\"\n", (int)tok.string.len,
				tok.string.value);
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
	};

	lex_finish(&lexer);
	return 0;
}
