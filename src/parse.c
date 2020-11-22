#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "identifier.h"
#include "lex.h"
#include "parse.h"
#include "types.h"
#include "utf8.h"

struct parser {
	struct lexer *lex;
};

static void
trace(struct parser *par, const char *name)
{
	if (getenv("HAREC_TRACE") == NULL) {
		return;
	}
	fprintf(stderr, "%s\n", name);
}

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
			fprintf(stderr, "%s", lexical_token_str(t));
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
	trace(par, "identifier");

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
	trace(par, "import");
	struct identifier ident = {0};
	parse_identifier(par, &ident);
	// TODO: Parse the various forms of imports
}

static void
parse_imports(struct parser *par, struct ast_subunit *subunit)
{
	trace(par, "imports");
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
