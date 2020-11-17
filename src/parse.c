#include <assert.h>
#include <stdio.h>
#include "ast.h"
#include "identifier.h"
#include "lex.h"
#include "parse.h"
#include "utf8.h"
#include "types.h"

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
			switch (tok.literal.storage) {
			case TYPE_STORAGE_U8:
			case TYPE_STORAGE_U16:
			case TYPE_STORAGE_U32:
			case TYPE_STORAGE_UINT:
			case TYPE_STORAGE_U64:
			case TYPE_STORAGE_SIZE:
				fprintf(stderr, "(%ju: %s)\n", tok.literal.u,
					type_storage_unparse(tok.literal.storage));
				break;
			case TYPE_STORAGE_I8:
			case TYPE_STORAGE_I16:
			case TYPE_STORAGE_I32:
			case TYPE_STORAGE_INT:
			case TYPE_STORAGE_I64:
				fprintf(stderr, "(%jd: %s)\n", tok.literal.s,
					type_storage_unparse(tok.literal.storage));
				break;
			case TYPE_STORAGE_F32:
			case TYPE_STORAGE_F64:
				fprintf(stderr, "(%lf: %s)\n", tok.literal.f,
					type_storage_unparse(tok.literal.storage));
				break;
			default:
				assert(0);
			}
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
