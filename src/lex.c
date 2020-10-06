#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lex.h"

// Gets the next non-whitespace character
static int
lwgetc(struct lexer *lexer)
{
	int c;
	while ((c = fgetc(lexer->in)) != EOF && isspace(c)) ;
	return c;
}

static int
lex_name(struct lexer *lexer, struct token *out)
{
	assert(0); // TODO
}

static int
lex_literal(struct lexer *lexer, struct token *out)
{
	assert(0); // TODO
}

void
lex_init(struct lexer *lexer, FILE *f)
{
	memset(lexer, 0, sizeof(*lexer));
	lexer->in = f;
}

int
lex(struct lexer *lexer, struct token *out)
{
	int c = lwgetc(lexer);
	if (c == EOF) {
		out->token = T_EOF;
		return c;
	}

	if (isalpha(c)) {
		// TODO: internal buffering
		ungetc(c, lexer->in);
		return lex_name(lexer, out);
	}

	if (isdigit(c)) {
		// TODO: internal buffering
		ungetc(c, lexer->in);
		return lex_literal(lexer, out);
	}

	switch (c) {
	case '.':
	case '"':
	case '\'':
		// TODO: internal buffering
		ungetc(c, lexer->in);
		return lex_literal(lexer, out);
	default:
		assert(0); // TODO: Operators
	}

	assert(0); // Unreachable
}

void
token_finish(struct token *tok)
{
	switch (tok->token) {
	case T_NAME:
		free(tok->name);
		break;
	default:
		break;
	}
}
