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
			switch (tok.storage) {
			case TYPE_STORAGE_F32:
			case TYPE_STORAGE_F64:
				fprintf(stderr, "(%lf: %s)\n", tok._float,
					type_storage_unparse(tok.storage));
				break;
			case TYPE_STORAGE_I8:
			case TYPE_STORAGE_I16:
			case TYPE_STORAGE_I32:
			case TYPE_STORAGE_I64:
			case TYPE_STORAGE_INT:
				fprintf(stderr, "(%jd: %s)\n", tok._signed,
					type_storage_unparse(tok.storage));
				break;
			case TYPE_STORAGE_RUNE:
				putc('\'', stderr);
				utf8_fputch(stderr, tok.rune);
				putc('\'', stderr);
				putc('\n', stderr);
				break;
			case TYPE_STORAGE_STRING:
				fprintf(stderr, "\"%*s\"\n", (int)tok.string.len,
					tok.string.value);
				break;
			case TYPE_STORAGE_SIZE:
			case TYPE_STORAGE_U8:
			case TYPE_STORAGE_U16:
			case TYPE_STORAGE_U32:
			case TYPE_STORAGE_U64:
			case TYPE_STORAGE_UINT:
				fprintf(stderr, "(%ju: %s)\n", tok._unsigned,
					type_storage_unparse(tok.storage));
				break;
			default:
				assert(0);
			}
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
