#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lex.h"

static const char *tokens[] = {
	// Must be alpha sorted
	[T_AS] = "as",
	[T_ASSERT] = "assert",
	[T_BOOL] = "bool",
	[T_BREAK] = "break",
	[T_CHAR] = "char",
	[T_CONST] = "const",
	[T_CONTINUE] = "continue",
	[T_DEF] = "def",
	[T_ELSE] = "else",
	[T_ENUM] = "enum",
	[T_EXPORT] = "export",
	[T_F32] = "f32",
	[T_F64] = "f64",
	[T_FALSE] = "false",
	[T_FN] = "fn",
	[T_FOR] = "for",
	[T_I16] = "i16",
	[T_I32] = "i32",
	[T_I64] = "i64",
	[T_I8] = "i8",
	[T_IF] = "if",
	[T_INT] = "int",
	[T_IS] = "is",
	[T_LEN] = "len",
	[T_LET] = "let",
	[T_MATCH] = "match",
	[T_NULL] = "null",
	[T_NULLABLE] = "nullable",
	[T_RETURN] = "return",
	[T_SIZE] = "size",
	[T_STATIC] = "static",
	[T_STR] = "str",
	[T_STRUCT] = "struct",
	[T_SWITCH] = "switch",
	[T_TRUE] = "true",
	[T_U16] = "u16",
	[T_U32] = "u32",
	[T_U64] = "u64",
	[T_U8] = "u8",
	[T_UINT] = "uint",
	[T_UINTPTR] = "uintptr",
	[T_UNION] = "union",
	[T_USE] = "use",
	[T_VOID] = "void",
	[T_WHILE] = "while",

	// Operators
	[T_ANDEQ] = "&=",
	[T_BAND] = "&",
	[T_BNOT] = "~",
	[T_BOR] = "|",
	[T_CASE] = "=>",
	[T_COLON] = ":",
	[T_COMMA] = ",",
	[T_DIV] = "/",
	[T_DIVEQ] = "/=",
	[T_DOT] = ".",
	[T_DOUBLE_COLON] = "::",
	[T_ELLIPSIS] = "...",
	[T_EQUAL] = "=",
	[T_GREATER] = ">",
	[T_GREATEREQ] = ">=",
	[T_LAND] = "&&",
	[T_LBRACE] = "{",
	[T_LBRACKET] = "[",
	[T_LEQUAL] = "==",
	[T_LESS] = "<",
	[T_LESSEQ] = "<=",
	[T_LNOT] = "!",
	[T_LOR] = "||",
	[T_LPAREN] = "(",
	[T_LSHIFT] = "<<",
	[T_LSHIFTEQ] = "<<=",
	[T_MINUS] = "-",
	[T_MINUSEQ] = "-=",
	[T_MINUSMINUS] = "--",
	[T_MODEQ] = "%=",
	[T_MODULO] = "%",
	[T_NEQUAL] = "!=",
	[T_OREQ] = "|=",
	[T_PLUS] = "+",
	[T_PLUSEQ] = "+=",
	[T_PLUSPLUS] = "++",
	[T_RBRACE] = "}",
	[T_RBRACKET] = "]",
	[T_RPAREN] = ")",
	[T_RSHIFT] = ">>",
	[T_RSHIFTEQ] = ">>=",
	[T_SEMICOLON] = ";",
	[T_SLICE] = "..",
	[T_TIMES] = "*",
	[T_TIMESEQ] = "*=",
	[T_XOR] = "^",
	[T_XOREQ] = "^=",
};

void
lex_init(struct lexer *lexer, FILE *f)
{
	memset(lexer, 0, sizeof(*lexer));
	lexer->in = f;
	lexer->bufsz = 256;
	lexer->buf = calloc(1, lexer->bufsz);
}

void
lex_finish(struct lexer *lexer)
{
	fclose(lexer->in);
	free(lexer->buf);
}

static int
next(struct lexer *lexer, bool buffer)
{
	int c;
	if (lexer->c != 0) {
		c = lexer->c;
		lexer->c = 0;
	} else {
		c = fgetc(lexer->in);
	}
	if (c == EOF || !buffer) {
		return c;
	}
	if (lexer->buflen + 1 >= lexer->bufsz) {
		lexer->bufsz *= 2;
		lexer->buf = realloc(lexer->buf, lexer->bufsz);
		assert(lexer->buf);
	}
	lexer->buf[lexer->buflen++] = c;
	lexer->buf[lexer->buflen] = '\0';
	return c;
}

static int
wgetc(struct lexer *lexer)
{
	int c;
	while ((c = next(lexer, false)) != EOF && isspace(c)) ;
	return c;
}

static void
consume(struct lexer *lexer, ssize_t n)
{
	if (n == -1) {
		lexer->buflen = 0;
		lexer->buf[0] = 0;
		return;
	}
	memmove(lexer->buf, &lexer->buf[lexer->buflen], lexer->buflen - n);
	lexer->buflen -= n;
}

static void
push(struct lexer *lexer, int c, bool buffer)
{
	lexer->c = c;
	if (buffer) {
		lexer->buf[--lexer->buflen] = 0;
	}
}

static int
cmp_keyword(const void *va, const void *vb)
{
	return strcmp(*(const char **)va, *(const char **)vb);
}

static int
lex_name(struct lexer *lexer, struct token *out)
{
	int c = next(lexer, true);
	if (c == EOF || (!isalpha(c) && c != '_')) {
		out->token = T_ERROR;
		return EOF;
	}
	while ((c = next(lexer, true)) != EOF) {
		if (!isalnum(c) && c != '_') {
			push(lexer, c, true);
			goto lookup;
		}
	}
	out->token = T_EOF;
	return c;

lookup:;
	void *token = bsearch(&lexer->buf, tokens, T_LAST_KEYWORD + 1,
			sizeof(tokens[0]), cmp_keyword);
	if (!token) {
		out->token = T_NAME;
		out->name = strdup(lexer->buf);
	} else {
		out->token = (const char **)token - tokens;
	}
	consume(lexer, -1);
	return c;
}

static int
lex_literal(struct lexer *lexer, struct token *out)
{
	assert(0); // TODO
}

static int
lex3(struct lexer *lexer, struct token *out, int c)
{
	assert(c != EOF);

	switch (c) {
	case '.':
		switch ((c = next(lexer, false))) {
		case '.':
			switch ((c = next(lexer, false))) {
			case '.':
				out->token = T_ELLIPSIS;
				break;
			default:
				push(lexer, c, false);
				out->token = T_SLICE;
				break;
			}
			break;
		default:
			push(lexer, c, false);
			out->token = T_DOT;
			break;
		}
		break;
	case '<':
		switch ((c = next(lexer, false))) {
		case '<':
			switch ((c = next(lexer, false))) {
			case '=':
				out->token = T_LSHIFTEQ;
				break;
			default:
				push(lexer, c, false);
				out->token = T_LSHIFT;
				break;
			}
			break;
		case '=':
			out->token = T_LESSEQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_LESS;
			break;
		}
		break;
	case '>':
		switch ((c = next(lexer, false))) {
		case '>':
			switch ((c = next(lexer, false))) {
			case '=':
				out->token = T_RSHIFTEQ;
				break;
			default:
				push(lexer, c, false);
				out->token = T_RSHIFT;
				break;
			}
			break;
		case '=':
			out->token = T_GREATEREQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_GREATER;
			break;
		}
		break;
	default:
		assert(0); // Invariant
	}

	return c;
}

static int
lex2(struct lexer *lexer, struct token *out, int c)
{
	assert(c != EOF);

	switch (c) {
	case '^':
		switch ((c = next(lexer, false))) {
		case '=':
			out->token = T_XOREQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_XOR;
			break;
		}
		break;
	case '*':
		switch ((c = next(lexer, false))) {
		case '=':
			out->token = T_TIMESEQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_TIMES;
			break;
		}
		break;
	case '%':
		switch ((c = next(lexer, false))) {
		case '=':
			out->token = T_MODEQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_MODULO;
			break;
		}
		break;
	case '/':
		switch ((c = next(lexer, false))) {
		case '=':
			out->token = T_DIVEQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_DIV;
			break;
		}
		break;
	case '+':
		switch ((c = next(lexer, false))) {
		case '=':
			out->token = T_PLUSEQ;
			break;
		case '+':
			out->token = T_PLUSPLUS;
			break;
		default:
			push(lexer, c, false);
			out->token = T_PLUS;
			break;
		}
		break;
	case '-':
		switch ((c = next(lexer, false))) {
		case '=':
			out->token = T_MINUSEQ;
			break;
		case '-':
			out->token = T_MINUSMINUS;
			break;
		default:
			push(lexer, c, false);
			out->token = T_MINUS;
			break;
		}
		break;
	case ':':
		switch (c) {
		case ':':
			out->token = T_DOUBLE_COLON;
			break;
		default:
			push(lexer, c, false);
			out->token = T_COLON;
			break;
		}
		break;
	case '!':
		switch ((c = next(lexer, false))) {
		case '=':
			out->token = T_NEQUAL;
			break;
		default:
			push(lexer, c, false);
			out->token = T_LNOT;
			break;
		}
		break;
	case '&':
		switch ((c = next(lexer, false))) {
		case '&':
			out->token = T_LAND;
			break;
		case '=':
			out->token = T_ANDEQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_BAND;
			break;
		}
		break;
	case '|':
		switch ((c = next(lexer, false))) {
		case '&':
			out->token = T_LOR;
			break;
		case '=':
			out->token = T_OREQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_BOR;
			break;
		}
		break;
	case '=':
		switch ((c = next(lexer, false))) {
		case '=':
			out->token = T_LEQUAL;
			break;
		default:
			push(lexer, c, false);
			out->token = T_EQUAL;
			break;
		}
		break;
	default:
		assert(0); // Invariant
	}

	return c;
}

int
lex(struct lexer *lexer, struct token *out)
{
	int c = wgetc(lexer);
	if (c == EOF) {
		out->token = T_EOF;
		return c;
	}

	if (isalpha(c)) {
		push(lexer, c, false);
		return lex_name(lexer, out);
	}

	if (isdigit(c)) {
		push(lexer, c, false);
		return lex_literal(lexer, out);
	}

	switch (c) {
	case '"':
	case '\'':
		push(lexer, c, false);
		return lex_literal(lexer, out);
	case '.': // . .. ...
	case '<': // < << <= <<=
	case '>': // > >> >= >>=
		return lex3(lexer, out, c);
	case '^': // ^ ^=
	case '*': // * *=
	case '%': // % %=
	case '/': // / /=
	case '+': // + +=
	case '-': // - -=
	case ':': // : ::
	case '!': // ! !=
	case '&': // & && &=
	case '|': // | || |=
	case '=': // = == =>
		return lex2(lexer, out, c);
	case '~':
		out->token = T_BNOT;
		break;
	case ',':
		out->token = T_COMMA;
		break;
	case '{':
		out->token = T_LBRACE;
		break;
	case '[':
		out->token = T_LBRACKET;
		break;
	case '(':
		out->token = T_LPAREN;
		break;
	case '}':
		out->token = T_RBRACE;
		break;
	case ']':
		out->token = T_RBRACKET;
		break;
	case ')':
		out->token = T_RPAREN;
		break;
	case ';':
		out->token = T_SEMICOLON;
		break;
	default:
		assert(0); // TODO: Operators
	}

	consume(lexer, 1);
	return c;
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
	tok->token = 0;
}

const char *
token_str(const struct token *tok)
{
	switch (tok->token) {
	case T_NAME:
		return tok->name;
	default:
		assert(tok->token < sizeof(tokens) / sizeof(tokens[0]));
		return tokens[tok->token];
	}
}
