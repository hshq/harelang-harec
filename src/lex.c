#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lex.h"
#include "utf8.h"

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
	[T_RUNE] = "rune",
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
	[T_LXOR] = "^^",
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

static uint32_t
next(struct lexer *lexer, bool buffer)
{
	uint32_t c;
	if (lexer->c[0] != 0) {
		c = lexer->c[0];
		lexer->c[0] = lexer->c[1];
		lexer->c[1] = 0;
	} else {
		c = utf8_fgetch(lexer->in);
	}
	if (c == UTF8_INVALID || !buffer) {
		return c;
	}
	if (lexer->buflen + utf8_chsize(c) >= lexer->bufsz) {
		lexer->bufsz *= 2;
		lexer->buf = realloc(lexer->buf, lexer->bufsz);
		assert(lexer->buf);
	}
	char buf[UTF8_MAX_SIZE];
	size_t sz = utf8_encode(&buf[0], c);
	memcpy(lexer->buf + lexer->buflen, buf, sz);
	lexer->buflen += sz;
	lexer->buf[lexer->buflen] = '\0';
	return c;
}

static uint32_t
wgetc(struct lexer *lexer)
{
	uint32_t c;
	while ((c = next(lexer, false)) != UTF8_INVALID && isspace(c)) ;
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
	for (ssize_t i = 0; i < n; i++) {
		while ((lexer->buf[--lexer->buflen] & 0xC0) == 0x80) ;
	}
	lexer->buf[lexer->buflen] = 0;
}

static void
push(struct lexer *lexer, uint32_t c, bool buffer)
{
	lexer->c[1] = lexer->c[0];
	lexer->c[0] = c;
	if (buffer) {
		consume(lexer, 1);
	}
}

static int
cmp_keyword(const void *va, const void *vb)
{
	return strcmp(*(const char **)va, *(const char **)vb);
}

static uint32_t
lex_name(struct lexer *lexer, struct token *out)
{
	uint32_t c = next(lexer, true);
	assert(c != UTF8_INVALID && c <= 0x7F && (isalpha(c) || c == '_'));
	while ((c = next(lexer, true)) != UTF8_INVALID) {
		if (c > 0x7F || (!isalnum(c) && c != '_')) {
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

static uint32_t
lex_literal(struct lexer *lexer, struct token *out)
{
	uint32_t c = next(lexer, true);
	assert(c != UTF8_INVALID && c <= 0x7F && isdigit(c));

	const char *base = "0123456789";
	switch ((c = next(lexer, true))) {
	case 'b':
		base = "01";
		break;
	case 'o':
		base = "01234567";
		break;
	case 'x':
		base = "0123456789ABCDEFabcdef";
		break;
	default:
		push(lexer, c, true);
		break;
	}

	char *suff = NULL;
	char *exp = NULL;
	bool isfloat = false;
	while ((c = next(lexer, true)) != UTF8_INVALID) {
		if (!strchr(base, c)) {
			switch (c) {
			case '.':
				if (isfloat || suff) {
					push(lexer, c, true);
					goto finalize;
				}
				isfloat = true;
				if (!strchr(base, c = next(lexer, false))) {
					push(lexer, c, false);
					push(lexer, '.', true);
					goto finalize;
				}
				push(lexer, c, false);
				break;
			case 'e':
				if (exp || suff) {
					push(lexer, c, true);
					goto finalize;
				}
				exp = &lexer->buf[lexer->buflen];
				break;
			case 'i':
			case 'u':
			case 'f':
			case 'z':
				if (suff) {
					push(lexer, c, true);
					goto finalize;
				}
				suff = &lexer->buf[lexer->buflen - 1];
				break;
			default:
				push(lexer, c, true);
				goto finalize;
			}
		}
	}

finalize:
	out->token = T_LITERAL;
	if (isfloat) {
		out->storage = TYPE_STORAGE_F64;
	} else {
		out->storage = TYPE_STORAGE_INT;
	}
	if (suff) {
		const char *suffs[] = {
			[TYPE_STORAGE_U8] = "u8",
			[TYPE_STORAGE_U16] = "u16",
			[TYPE_STORAGE_U32] = "u32",
			[TYPE_STORAGE_U64] = "u64",
			[TYPE_STORAGE_I8] = "i8",
			[TYPE_STORAGE_I16] = "i16",
			[TYPE_STORAGE_I32] = "i32",
			[TYPE_STORAGE_I64] = "i64",
		
			[TYPE_STORAGE_UINT] = "u",
			[TYPE_STORAGE_INT] = "i",
			[TYPE_STORAGE_SIZE] = "z",
			[TYPE_STORAGE_F32] = "f32",
			[TYPE_STORAGE_F64] = "f64",
		};
		bool isvalid = false;
		for (enum type_storage i = 0;
				i < sizeof(suffs) / sizeof(suffs[0]); ++i) {
			if (suffs[i] && strcmp(suff, suffs[i]) == 0) {
				isvalid = true;
				out->storage = i;
				break;
			}
		}
		if (!isvalid) {
			out->token = T_ERROR;
			consume(lexer, -1);
			return c;
		}
	}

	uintmax_t exponent = 0;
	if (exp) {
		char *endptr = NULL;
		exponent = strtoumax(exp, &endptr, 10);
		if (endptr == exp) {
			out->token = T_ERROR;
			consume(lexer, -1);
			return c;
		}
	}

	errno = 0;
	switch (out->storage) {
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_SIZE:
		out->_unsigned = strtoumax(lexer->buf, NULL, strlen(base));
		for (uintmax_t i = 0; i < exponent; i++) {
			out->_unsigned *= 10;
		}
		break;
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_I64:
		out->_signed = strtoimax(lexer->buf, NULL, strlen(base));
		for (uintmax_t i = 0; i < exponent; i++) {
			out->_signed *= 10;
		}
		break;
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		out->_float = strtod(lexer->buf, NULL);
		break;
	default:
		assert(0);
	}
	if (errno == ERANGE) {
		out->token = T_ERROR;
	}
	consume(lexer, -1);
	return c;
}

static uint32_t
lex_rune(struct lexer *lexer)
{
	char buf[5];
	char *endptr;
	uint32_t c = next(lexer, false);
	assert(c != UTF8_INVALID);

	switch (c) {
	case '\\':
		c = next(lexer, false);
		switch (c) {
		case '0':
			return '\0';
		case 'a':
			return '\a';
		case 'b':
			return '\b';
		case 'f':
			return '\f';
		case 'n':
			return '\n';
		case 'r':
			return '\r';
		case 't':
			return '\t';
		case 'v':
			return '\v';
		case '\\':
			return '\\';
		case '\'':
			return '\'';
		case '"':
			return '\"';
		case 'x':
			buf[0] = next(lexer, false);
			buf[1] = next(lexer, false);
			buf[2] = '\0';
			c = strtoul(&buf[0], &endptr, 16);
			assert(*endptr == '\0');
			return c;
		case 'u':
			buf[0] = next(lexer, false);
			buf[1] = next(lexer, false);
			buf[2] = next(lexer, false);
			buf[3] = next(lexer, false);
			buf[4] = '\0';
			c = strtoul(&buf[0], &endptr, 16);
			assert(*endptr == '\0');
			return c;
		default:
			assert(0); // Invariant
		}
		assert(0);
	default:
		return c;
	}
	assert(0);
}

static uint32_t
lex_string(struct lexer *lexer, struct token *out)
{
	uint32_t c = next(lexer, false);
	assert(c != UTF8_INVALID);

	switch (c) {
	case '"':
		while ((c = next(lexer, false)) != UTF8_INVALID) {
			switch (c) {
			case '"':;
				char *buf = malloc(lexer->buflen);
				memcpy(buf, lexer->buf, lexer->buflen);
				out->token = T_LITERAL;
				out->storage = TYPE_STORAGE_STRING;
				out->string.len = lexer->buflen;
				out->string.value = buf;
				consume(lexer, -1);
				return c;
			default:
				push(lexer, c, false);
				push(lexer, lex_rune(lexer), false);
				next(lexer, true);
			}
		}
		assert(0); // Invariant
	case '\'':
		c = next(lexer, false);
		switch (c) {
		case '\'':
			assert(0); // Invariant
		case '\\':
			push(lexer, c, false);
			out->rune = lex_rune(lexer);
			break;
		default:
			out->rune = c;
		}
		c = next(lexer, false);
		assert(c == '\'');
		out->token = T_LITERAL;
		out->storage = TYPE_STORAGE_RUNE;
		return c;
	default:
		assert(0); // Invariant
	}
	assert(0);
}

static uint32_t
lex3(struct lexer *lexer, struct token *out, uint32_t c)
{
	assert(c != UTF8_INVALID);

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

static uint32_t
lex2(struct lexer *lexer, struct token *out, uint32_t c)
{
	assert(c != UTF8_INVALID);

	switch (c) {
	case '^':
		switch ((c = next(lexer, false))) {
		case '^':
			out->token = T_LXOR;
			break;
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
		case '/':
			while ((c = next(lexer, false)) != UTF8_INVALID && c != '\n') ;
			return lex(lexer, out);
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

uint32_t
lex(struct lexer *lexer, struct token *out)
{
	uint32_t c = wgetc(lexer);
	if (c == UTF8_INVALID) {
		out->token = T_EOF;
		return c;
	}

	if (c <= 0x7F && (isalpha(c) || c == '_')) {
		push(lexer, c, false);
		return lex_name(lexer, out);
	}

	if (c <= 0x7F && isdigit(c)) {
		push(lexer, c, false);
		return lex_literal(lexer, out);
	}

	switch (c) {
	case '"':
	case '\'':
		push(lexer, c, false);
		return lex_string(lexer, out);
	case '.': // . .. ...
	case '<': // < << <= <<=
	case '>': // > >> >= >>=
		return lex3(lexer, out, c);
	case '^': // ^ ^=
	case '*': // * *=
	case '%': // % %=
	case '/': // / /= //
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
		out->token = T_ERROR;
		break;
	}

	return c;
}

void
token_finish(struct token *tok)
{
	switch (tok->token) {
	case T_NAME:
		free(tok->name);
		break;
	case T_LITERAL:
		switch (tok->storage) {
		case TYPE_STORAGE_STRING:
			free(tok->string.value);
			break;
		default:
			break;
		}
		break;
	default:
		break;
	}
	tok->token = 0;
	tok->storage = 0;
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
