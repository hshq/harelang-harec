#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "identifier.h"
#include "lex.h"
#include "parse.h"
#include "trace.h"
#include "types.h"
#include "utf8.h"

struct parser {
	struct lexer *lex;
};

static void
synassert(bool cond, struct token *tok, ...)
{
	if (!cond) {
		va_list ap;
		va_start(ap, tok);

		// TODO: file name, lineno, colno
		enum lexical_token t = va_arg(ap, enum lexical_token);
		fprintf(stderr,
			"Syntax error: unexpected '%s'%s",
			token_str(tok),
			t == T_EOF ? "\n" : ", expected " );
		while (t != T_EOF) {
			if (t == T_LITERAL || t == T_NAME) {
				fprintf(stderr, "%s", lexical_token_str(t));
			} else {
				fprintf(stderr, "'%s'", lexical_token_str(t));
			}
			t = va_arg(ap, enum lexical_token);
			fprintf(stderr, "%s", t == T_EOF ? "\n" : ", ");
		}
		exit(1);
	}
}

static void
want(struct parser *par, enum lexical_token ltok, struct token *tok)
{
	struct token _tok = {0};
	lex(par->lex, tok ? tok : &_tok);
	synassert(tok->token == ltok, tok ? tok : &_tok, ltok, T_EOF);
	if (!tok) {
		token_finish(&_tok);
	}
}

static void
parse_identifier(struct parser *par, struct identifier *ident)
{
	struct token tok = {0};
	struct identifier *i = ident;
	trace("parse", "identifier");

	while (true) {
		want(par, T_NAME, &tok);
		i->name = strdup(tok.name);
		token_finish(&tok);

		struct identifier *ns;
		switch (lex(par->lex, &tok)) {
		case T_DOUBLE_COLON:
			ns = calloc(1, sizeof(struct identifier));
			*ns = *i;
			i->ns = ns;
			i = ns;
			break;
		default:
			// TODO: Unlex
			return;
		}
	}
}

static void
parse_import(struct parser *par, struct ast_imports *imports)
{
	trace("parse", "import");
	struct identifier ident = {0};
	parse_identifier(par, &ident);

	struct token tok = {0};
	switch (lex(par->lex, &tok)) {
	case T_EQUAL:
		assert(0); // TODO
	case T_LBRACE:
		assert(0); // TODO
	case T_SEMICOLON:
		imports->mode = AST_IMPORT_IDENTIFIER;
		imports->ident = ident;
		break;
	default:
		synassert(false, &tok, T_EQUAL, T_LBRACE, T_SEMICOLON, T_EOF);
		break;
	}
}

static void
parse_imports(struct parser *par, struct ast_subunit *subunit)
{
	trace("parse", "imports");
	struct token tok = {0};
	struct ast_imports **next = &subunit->imports;

	while (true) {
		struct ast_imports *imports;
		switch (lex(par->lex, &tok)) {
		case T_USE:
			imports = calloc(1, sizeof(struct ast_imports));
			parse_import(par, imports);
			*next = imports;
			next = &imports->next;
			break;
		default:
			// TODO: unlex
			return;
		}
	}
}

void
parse(struct lexer *lex, struct ast_subunit *subunit)
{
	struct parser par = {
		.lex = lex,
	};
	parse_imports(&par, subunit);
}
