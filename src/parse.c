#include <stdio.h>
#include "ast.h"
#include "identifier.h"
#include "lex.h"
#include "parse.h"
#include "utf8.h"

void
parse(struct lexer *lexer, struct identifier *ns, struct ast_unit *unit)
{
	struct token tok = {0};

	while (tok.token != T_EOF) {
		token_finish(&tok);
		lex(lexer, &tok);
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

	token_finish(&tok);
}
