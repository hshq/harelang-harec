#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include "lex.h"
#include "utf8.h"
#include "util.h"

static const char *tokens[] = {
	// Must be alpha sorted and match lex.h
	[T_ATTR_FINI] = "@fini",
	[T_ATTR_INIT] = "@init",
	[T_ATTR_NORETURN] = "@noreturn",
	[T_ATTR_OFFSET] = "@offset",
	[T_ATTR_PACKED] = "@packed",
	[T_ATTR_SYMBOL] = "@symbol",
	[T_ATTR_TEST] = "@test",
	[T_ATTR_THREADLOCAL] = "@threadlocal",
	[T_UNDERSCORE] = "_",
	[T_ABORT] = "abort",
	[T_ALLOC] = "alloc",
	[T_APPEND] = "append",
	[T_AS] = "as",
	[T_ASSERT] = "assert",
	[T_BOOL] = "bool",
	[T_BREAK] = "break",
	[T_CASE] = "case",
	[T_CHAR] = "char",
	[T_CONST] = "const",
	[T_CONTINUE] = "continue",
	[T_DEFER] = "defer",
	[T_DEF] = "def",
	[T_DELETE] = "delete",
	[T_ELSE] = "else",
	[T_ENUM] = "enum",
	[T_EXPORT] = "export",
	[T_F32] = "f32",
	[T_F64] = "f64",
	[T_FALSE] = "false",
	[T_FN] = "fn",
	[T_FOR] = "for",
	[T_FREE] = "free",
	[T_I16] = "i16",
	[T_I32] = "i32",
	[T_I64] = "i64",
	[T_I8] = "i8",
	[T_IF] = "if",
	[T_INSERT] = "insert",
	[T_INT] = "int",
	[T_IS] = "is",
	[T_LEN] = "len",
	[T_LET] = "let",
	[T_MATCH] = "match",
	[T_NULL] = "null",
	[T_NULLABLE] = "nullable",
	[T_OFFSET] = "offset",
	[T_RETURN] = "return",
	[T_RUNE] = "rune",
	[T_SIZE] = "size",
	[T_STATIC] = "static",
	[T_STR] = "str",
	[T_STRUCT] = "struct",
	[T_SWITCH] = "switch",
	[T_TRUE] = "true",
	[T_TYPE] = "type",
	[T_U16] = "u16",
	[T_U32] = "u32",
	[T_U64] = "u64",
	[T_U8] = "u8",
	[T_UINT] = "uint",
	[T_UINTPTR] = "uintptr",
	[T_UNION] = "union",
	[T_USE] = "use",
	[T_VAARG] = "vaarg",
	[T_VAEND] = "vaend",
	[T_VALIST] = "valist",
	[T_VASTART] = "vastart",
	[T_VOID] = "void",
	[T_YIELD] = "yield",

	// Operators
	[T_ARROW] = "=>",
	[T_BANDEQ] = "&=",
	[T_BAND] = "&",
	[T_BNOT] = "~",
	[T_BOR] = "|",
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
	[T_LANDEQ] = "&&=",
	[T_LBRACE] = "{",
	[T_LBRACKET] = "[",
	[T_LEQUAL] = "==",
	[T_LESS] = "<",
	[T_LESSEQ] = "<=",
	[T_LNOT] = "!",
	[T_LOR] = "||",
	[T_LOREQ] = "||=",
	[T_LPAREN] = "(",
	[T_LSHIFT] = "<<",
	[T_LSHIFTEQ] = "<<=",
	[T_LXOR] = "^^",
	[T_LXOREQ] = "^^=",
	[T_MINUS] = "-",
	[T_MINUSEQ] = "-=",
	[T_MODEQ] = "%=",
	[T_MODULO] = "%",
	[T_NEQUAL] = "!=",
	[T_BOREQ] = "|=",
	[T_PLUS] = "+",
	[T_PLUSEQ] = "+=",
	[T_QUESTION] = "?",
	[T_RBRACE] = "}",
	[T_RBRACKET] = "]",
	[T_RPAREN] = ")",
	[T_RSHIFT] = ">>",
	[T_RSHIFTEQ] = ">>=",
	[T_SEMICOLON] = ";",
	[T_SLICE] = "..",
	[T_TIMES] = "*",
	[T_TIMESEQ] = "*=",
	[T_BXOR] = "^",
	[T_BXOREQ] = "^=",
};

static noreturn void
error(struct location *loc, char *fmt, ...)
{
	fprintf(stderr, "Syntax error at %s:%d:%d: ", sources[loc->file],
			loc->lineno, loc->colno);

	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	fputc('\n', stderr);
	exit(EXIT_FAILURE);
}

void
lex_init(struct lexer *lexer, FILE *f, int fileid)
{
	memset(lexer, 0, sizeof(*lexer));
	lexer->in = f;
	lexer->bufsz = 256;
	lexer->buf = xcalloc(1, lexer->bufsz);
	lexer->un.token = T_ERROR;
	lexer->loc.lineno = 1;
	lexer->loc.colno = 0;
	lexer->loc.file = fileid;
	lexer->c[0] = UINT32_MAX;
	lexer->c[1] = UINT32_MAX;
}

void
lex_finish(struct lexer *lexer)
{
	fclose(lexer->in);
	free(lexer->buf);
}

static void
update_lineno(struct location *loc, uint32_t c)
{
	if (c == '\n') {
		loc->lineno++;
		loc->colno = 0;
	} else if (c == '\t') {
		loc->colno += 8;
	} else {
		loc->colno++;
	}
}

static uint32_t
next(struct lexer *lexer, struct location *loc, bool buffer)
{
	uint32_t c;
	if (lexer->c[0] != UINT32_MAX) {
		c = lexer->c[0];
		lexer->c[0] = lexer->c[1];
		lexer->c[1] = UINT32_MAX;
	} else {
		c = utf8_get(lexer->in);
		update_lineno(&lexer->loc, c);
		if (c == UTF8_INVALID && !feof(lexer->in)) {
			error(&lexer->loc, "Invalid UTF-8 sequence encountered");
		}
	}
	if (loc != NULL) {
		*loc = lexer->loc;
		for (size_t i = 0; i < 2 && lexer->c[i] != UINT32_MAX; i++) {
			update_lineno(&lexer->loc, lexer->c[i]);
		}
	}
	if (c == C_EOF || !buffer) {
		return c;
	}
	if (lexer->buflen + utf8_cpsize(c) >= lexer->bufsz) {
		lexer->bufsz *= 2;
		lexer->buf = xrealloc(lexer->buf, lexer->bufsz);
	}
	char buf[UTF8_MAX_SIZE];
	size_t sz = utf8_encode(&buf[0], c);
	memcpy(lexer->buf + lexer->buflen, buf, sz);
	lexer->buflen += sz;
	lexer->buf[lexer->buflen] = '\0';
	return c;
}

static bool
isharespace(uint32_t c)
{
	return c == '\t' || c == '\n' || c == ' ';
}

static uint32_t
wgetc(struct lexer *lexer, struct location *loc)
{
	uint32_t c;
	while ((c = next(lexer, loc, false)) != C_EOF && isharespace(c)) ;
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
	assert(lexer->c[1] == UINT32_MAX);
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
	uint32_t c = next(lexer, &out->loc, true);
	assert(c != C_EOF && c <= 0x7F && (isalpha(c) || c == '_' || c == '@'));
	while ((c = next(lexer, NULL, true)) != C_EOF) {
		if (c > 0x7F || (!isalnum(c) && c != '_')) {
			push(lexer, c, true);
			break;
		}
	}

	void *token = bsearch(&lexer->buf, tokens, T_LAST_KEYWORD + 1,
			sizeof(tokens[0]), cmp_keyword);
	if (!token) {
		if (lexer->buf[0] == '@') {
			error(&out->loc, "Unknown attribute %s", lexer->buf);
		}
		out->token = T_NAME;
		out->name = xstrdup(lexer->buf);
	} else {
		out->token = (const char **)token - tokens;
	}
	consume(lexer, -1);
	return out->token;
}

static uintmax_t
compute_exp(uintmax_t n, int exponent, bool _signed, struct location *loc)
{
	if (n == 0) {
		return 0;
	}
	for (int i = 0; i < exponent; i++) {
		uintmax_t old = n;
		n *= 10;
		if (n / 10 != old) {
			error(loc, "Integer literal overflow");
		}
	}
	if (_signed && n > (uintmax_t)INT64_MIN) {
		error(loc, "Integer literal overflow");
	}
	return n;
}

static uint32_t
lex_literal(struct lexer *lexer, struct token *out)
{
	uint32_t c = next(lexer, &out->loc, true);
	assert(c != C_EOF && c <= 0x7F && isdigit(c));

	bool started = false;
	int base = 10;
	const char *basechrs = "0123456789";
	if (c == '0') {
		switch ((c = next(lexer, NULL, true))) {
		case 'b':
			base = 2;
			basechrs = "01";
			consume(lexer, 2);
			break;
		case 'o':
			base = 8;
			basechrs = "01234567";
			consume(lexer, 2);
			break;
		case 'x':
			base = 16;
			basechrs = "0123456789ABCDEFabcdef";
			consume(lexer, 2);
			break;
		default:
			started = true;
			push(lexer, c, true);
			break;
		}
	} else {
		started = true;
	}

	char *suff = NULL;
	char *exp = NULL;
	bool isfloat = false;
	while ((c = next(lexer, NULL, true)) != C_EOF) {
		if (!strchr(basechrs, c)) {
			switch (c) {
			case '.':
				if (!started) {
					push(lexer, c, true);
					goto finalize;
				}
				if (lexer->require_int) {
					push(lexer, '.', true);
					goto finalize;
				}
				if (isfloat || suff || exp) {
					push(lexer, c, true);
					goto finalize;
				}
				if (!strchr(basechrs, c = next(lexer, NULL, false))) {
					push(lexer, c, false);
					push(lexer, '.', true);
					goto finalize;
				} else {
					push(lexer, c, false);
				}
				isfloat = true;
				break;
			case 'e':
				if (!started) {
					push(lexer, c, true);
					goto finalize;
				}
				if (exp || suff) {
					push(lexer, c, true);
					goto finalize;
				}
				// exponent is always in base 10
				basechrs = "0123456789";
				c = next(lexer, NULL, true);
				if (c != '-' && c != '+' && !strchr(basechrs, c)) {
					push(lexer, c, true);
					push(lexer, 'e', true);
					goto finalize;
				};
				exp = &lexer->buf[lexer->buflen - 1];
				break;
			case 'f':
				if (base != 10) {
					push(lexer, c, true);
					goto finalize;
				}
				// Fallthrough
			case 'i':
			case 'u':
			case 'z':
				if (suff || !started) {
					push(lexer, c, true);
					goto finalize;
				}
				suff = &lexer->buf[lexer->buflen - 1];
				basechrs = "0123456789";
				break;
			default:
				push(lexer, c, true);
				goto finalize;
			}
		}
		started = true;
	}

finalize:
	if (!started) {
		error(&out->loc, "Invalid literal");
	}
	lexer->require_int = false;
	out->token = T_LITERAL;
	if (isfloat) {
		out->storage = STORAGE_FCONST;
	} else {
		out->storage = STORAGE_ICONST;
	}
	if (suff) {
		const char *suffs[] = {
			[STORAGE_U8] = "u8",
			[STORAGE_U16] = "u16",
			[STORAGE_U32] = "u32",
			[STORAGE_U64] = "u64",
			[STORAGE_I8] = "i8",
			[STORAGE_I16] = "i16",
			[STORAGE_I32] = "i32",
			[STORAGE_I64] = "i64",

			[STORAGE_UINT] = "u",
			[STORAGE_INT] = "i",
			[STORAGE_SIZE] = "z",
			[STORAGE_F32] = "f32",
			[STORAGE_F64] = "f64",
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
			error(&out->loc, "Invalid numeric suffix");
		}
	}

	intmax_t exponent = 0;
	if (exp) {
		char *endptr = NULL;
		errno = 0;
		exponent = strtoimax(exp, &endptr, 10);
		if (errno == ERANGE) {
			error(&out->loc, "Numerical exponent overflow");
		}
		// integers can't have negative exponents
		if (exponent < 0 && !suff) {
			out->storage = STORAGE_FCONST;
		}
		enum type_storage s = out->storage;
		bool valid = exponent >= 0
			|| s == STORAGE_F32
			|| s == STORAGE_F64
			|| s == STORAGE_FCONST;
		if (endptr == exp || !valid) {
			error(&out->loc, "Integers cannot have negative exponents");
		}
	}

	if (isfloat) {
		switch (out->storage) {
		case STORAGE_F32:
		case STORAGE_F64:
		case STORAGE_FCONST:
			break;
		default:
			error(&out->loc, "Unexpected decimal point in integer literal");
		}
	}

	errno = 0;
	switch (out->storage) {
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_UINT:
	case STORAGE_U64:
	case STORAGE_SIZE:
		out->uval = compute_exp(strtoumax(lexer->buf, NULL, base),
				exponent, false, &out->loc);
		break;
	case STORAGE_ICONST:
		out->uval = compute_exp(strtoumax(lexer->buf, NULL, base),
				exponent, false, &out->loc);
		if (out->uval > (uintmax_t)INT64_MAX) {
			out->storage = STORAGE_U64;
			break;
		}
		// Fallthrough
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_INT:
	case STORAGE_I64:
		out->uval = compute_exp(strtoumax(lexer->buf, NULL, base),
				exponent, true, &out->loc);
		if (out->uval == (uintmax_t)INT64_MIN) {
			// XXX: Hack
			out->ival = INT64_MIN;
		} else {
			out->ival = (intmax_t)out->uval;
		}
		break;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		out->fval = strtod(lexer->buf, NULL);
		break;
	default:
		assert(0);
	}
	if (errno == ERANGE && !isfloat) {
		error(&out->loc, "Integer literal overflow");
	}
	consume(lexer, -1);
	return out->token;
}

static uint32_t
lex_rune(struct lexer *lexer)
{
	char buf[9];
	char *endptr;
	uint32_t c = next(lexer, NULL, false);
	assert(c != C_EOF);

	switch (c) {
	case '\\':
		c = next(lexer, NULL, false);
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
			buf[0] = next(lexer, NULL, false);
			buf[1] = next(lexer, NULL, false);
			buf[2] = '\0';
			c = strtoul(&buf[0], &endptr, 16);
			if (*endptr != '\0') {
				error(&lexer->loc, "Invalid hex literal");
			}
			return c;
		case 'u':
			buf[0] = next(lexer, NULL, false);
			buf[1] = next(lexer, NULL, false);
			buf[2] = next(lexer, NULL, false);
			buf[3] = next(lexer, NULL, false);
			buf[4] = '\0';
			c = strtoul(&buf[0], &endptr, 16);
			if (*endptr != '\0') {
				error(&lexer->loc, "Invalid hex literal");
			}
			return c;
		case 'U':
			buf[0] = next(lexer, NULL, false);
			buf[1] = next(lexer, NULL, false);
			buf[2] = next(lexer, NULL, false);
			buf[3] = next(lexer, NULL, false);
			buf[4] = next(lexer, NULL, false);
			buf[5] = next(lexer, NULL, false);
			buf[6] = next(lexer, NULL, false);
			buf[7] = next(lexer, NULL, false);
			buf[8] = '\0';
			c = strtoul(&buf[0], &endptr, 16);
			if (*endptr != '\0') {
				error(&lexer->loc, "Invalid hex literal");
			}
			return c;
		case C_EOF:
			error(&lexer->loc, "Unexpected end of file");
		default:
			error(&lexer->loc, "Invalid escape '\\%c'", c);
		}
		assert(0);
	default:
		return c;
	}
	assert(0);
}

static enum lexical_token
lex_string(struct lexer *lexer, struct token *out)
{
	uint32_t c = next(lexer, &out->loc, false);
	uint32_t delim;

	switch (c) {
	case '"':
	case '`':
		delim = c;
		while ((c = next(lexer, NULL, false)) != delim) {
			if (c == C_EOF) {
				error(&lexer->loc, "Unexpected end of file");
			}
			push(lexer, c, false);
			if (delim == '"') {
				push(lexer, lex_rune(lexer), false);
			}
			next(lexer, NULL, true);
		}
		char *buf = xcalloc(lexer->buflen + 1, 1);
		memcpy(buf, lexer->buf, lexer->buflen);
		out->token = T_LITERAL;
		out->storage = STORAGE_STRING;
		out->string.len = lexer->buflen;
		out->string.value = buf;
		consume(lexer, -1);
		return out->token;
	case '\'':
		c = next(lexer, NULL, false);
		switch (c) {
		case '\'':
			error(&out->loc, "Expected rune before trailing single quote");
		case '\\':
			push(lexer, c, false);
			out->rune = lex_rune(lexer);
			break;
		default:
			out->rune = c;
		}
		if (next(lexer, NULL, false) != '\'') {
			error(&out->loc, "Expected trailing single quote");
		}
		out->token = T_LITERAL;
		out->storage = STORAGE_RCONST;
		return out->token;
	default:
		assert(0); // Invariant
	}
	assert(0);
}

static enum lexical_token
lex3(struct lexer *lexer, struct token *out, uint32_t c)
{
	assert(c != C_EOF);

	switch (c) {
	case '.':
		switch ((c = next(lexer, NULL, false))) {
		case '.':
			switch ((c = next(lexer, NULL, false))) {
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
			lexer->require_int = true;
			break;
		}
		break;
	case '<':
		switch ((c = next(lexer, NULL, false))) {
		case '<':
			switch ((c = next(lexer, NULL, false))) {
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
		switch ((c = next(lexer, NULL, false))) {
		case '>':
			switch ((c = next(lexer, NULL, false))) {
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
	case '&':
		switch ((c = next(lexer, NULL, false))) {
		case '&':
			switch ((c = next(lexer, NULL, false))) {
			case '=':
				out->token = T_LANDEQ;
				break;
			default:
				push(lexer, c, false);
				out->token = T_LAND;
				break;
			}
			break;
		case '=':
			out->token = T_BANDEQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_BAND;
			break;
		}
		break;
	case '|':
		switch ((c = next(lexer, NULL, false))) {
		case '|':
			switch ((c = next(lexer, NULL, false))) {
			case '=':
				out->token = T_LOREQ;
				break;
			default:
				push(lexer, c, false);
				out->token = T_LOR;
				break;
			}
			break;
		case '=':
			out->token = T_BOREQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_BOR;
			break;
		}
		break;
	case '^':
		switch ((c = next(lexer, NULL, false))) {
		case '^':
			switch ((c = next(lexer, NULL, false))) {
			case '=':
				out->token = T_LXOREQ;
				break;
			default:
				push(lexer, c, false);
				out->token = T_LXOR;
				break;
			}
			break;
		case '=':
			out->token = T_BXOREQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_BXOR;
			break;
		}
		break;
	default:
		assert(0); // Invariant
	}

	return out->token;
}

static enum lexical_token _lex(struct lexer *lexer, struct token *out);

static enum lexical_token
lex2(struct lexer *lexer, struct token *out, uint32_t c)
{
	assert(c != C_EOF);

	switch (c) {
	case '*':
		switch ((c = next(lexer, NULL, false))) {
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
		switch ((c = next(lexer, NULL, false))) {
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
		switch ((c = next(lexer, NULL, false))) {
		case '=':
			out->token = T_DIVEQ;
			break;
		case '/':
			while ((c = next(lexer, NULL, false)) != C_EOF && c != '\n') ;
			return _lex(lexer, out);
		default:
			push(lexer, c, false);
			out->token = T_DIV;
			break;
		}
		break;
	case '+':
		switch ((c = next(lexer, NULL, false))) {
		case '=':
			out->token = T_PLUSEQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_PLUS;
			break;
		}
		break;
	case '-':
		switch ((c = next(lexer, NULL, false))) {
		case '=':
			out->token = T_MINUSEQ;
			break;
		default:
			push(lexer, c, false);
			out->token = T_MINUS;
			break;
		}
		break;
	case ':':
		switch ((c = next(lexer, NULL, false))) {
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
		switch ((c = next(lexer, NULL, false))) {
		case '=':
			out->token = T_NEQUAL;
			break;
		default:
			push(lexer, c, false);
			out->token = T_LNOT;
			break;
		}
		break;
	case '=':
		switch ((c = next(lexer, NULL, false))) {
		case '=':
			out->token = T_LEQUAL;
			break;
		case '>':
			out->token = T_ARROW;
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

	return out->token;
}

static enum lexical_token
_lex(struct lexer *lexer, struct token *out)
{
	if (lexer->un.token != T_ERROR) {
		*out = lexer->un;
		lexer->un.token = T_ERROR;
		return out->token;
	}

	uint32_t c = wgetc(lexer, &out->loc);
	if (c == C_EOF) {
		out->token = T_EOF;
		return out->token;
	}

	if (c <= 0x7F && isdigit(c)) {
		push(lexer, c, false);
		return lex_literal(lexer, out);
	}

	lexer->require_int = false;

	if (c <= 0x7F && (isalpha(c) || c == '_' || c == '@')) {
		push(lexer, c, false);
		return lex_name(lexer, out);
	}

	char p[5];
	switch (c) {
	case '"':
	case '`':
	case '\'':
		push(lexer, c, false);
		return lex_string(lexer, out);
	case '.': // . .. ...
	case '<': // < << <= <<=
	case '>': // > >> >= >>=
	case '&': // & && &= &&=
	case '|': // | || |= ||=
	case '^': // ^ ^^ ^= ^^=
		return lex3(lexer, out, c);
	case '*': // * *=
	case '%': // % %=
	case '/': // / /= //
	case '+': // + +=
	case '-': // - -=
	case ':': // : ::
	case '!': // ! !=
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
	case '?':
		out->token = T_QUESTION;
		break;
	default:
		p[utf8_encode(p, c)] = '\0';
		fprintf(stderr, "Error: unexpected code point '%s' at %s:%d:%d\n",
			p, sources[lexer->loc.file], lexer->loc.lineno,
			lexer->loc.colno);
		exit(EXIT_FAILURE);
	}

	return out->token;
}

enum lexical_token
lex(struct lexer *lexer, struct token *out)
{
	return _lex(lexer, out);
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
		case STORAGE_STRING:
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
	tok->loc.file = 0;
	tok->loc.colno = 0;
	tok->loc.lineno = 0;
}

const char *
lexical_token_str(enum lexical_token tok)
{
	switch (tok) {
	case T_NAME:
		return "name";
	case T_LITERAL:
		return "literal";
	case T_EOF:
		return "end of file";
	case T_ERROR:
		return "error";
	default:
		assert(tok < sizeof(tokens) / sizeof(tokens[0]));
		return tokens[tok];
	}
}

static const char *
rune_unparse(uint32_t c)
{
	static char buf[11];
	switch (c) {
	case '\0':
		snprintf(buf, sizeof(buf), "\\0");
		break;
	case '\a':
		snprintf(buf, sizeof(buf), "\\a");
		break;
	case '\b':
		snprintf(buf, sizeof(buf), "\\b");
		break;
	case '\f':
		snprintf(buf, sizeof(buf), "\\f");
		break;
	case '\n':
		snprintf(buf, sizeof(buf), "\\n");
		break;
	case '\r':
		snprintf(buf, sizeof(buf), "\\r");
		break;
	case '\t':
		snprintf(buf, sizeof(buf), "\\t");
		break;
	case '\v':
		snprintf(buf, sizeof(buf), "\\v");
		break;
	case '\\':
		snprintf(buf, sizeof(buf), "\\\\");
		break;
	case '\'':
		snprintf(buf, sizeof(buf), "\\'");
		break;
	case '"':
		snprintf(buf, sizeof(buf), "\\\"");
		break;
	default:
		if (c > 0xffff) {
			snprintf(buf, sizeof(buf), "\\U%08x", c);
		} else if (c > 0x7F) {
			snprintf(buf, sizeof(buf), "\\u%04x", c);
		} else if (!isprint(c)) {
			snprintf(buf, sizeof(buf), "\\x%02x", c);
		} else {
			assert(utf8_cpsize(c) < sizeof(buf));
			buf[utf8_encode(buf, c)] = '\0';
		}
		break;
	}
	return buf;
}

static const char *
string_unparse(const struct token *tok)
{
	static char buf[1024];
	assert(tok->token == T_LITERAL && tok->storage == STORAGE_STRING);
	int bytes = 0;
	memset(buf, 0, sizeof(buf));
	bytes += snprintf(&buf[bytes], sizeof(buf) - bytes, "\"");
	const char *s = tok->string.value;
	for (uint32_t c = utf8_decode(&s);
			s - tok->string.value <= (ptrdiff_t)tok->string.len;
			c = utf8_decode(&s)) {
		bytes += snprintf(&buf[bytes], sizeof(buf) - bytes, "%s",
			rune_unparse(c));
	}
	bytes += snprintf(&buf[bytes], sizeof(buf) - bytes, "\"");
	return buf;
}

const char *
token_str(const struct token *tok)
{
	static char buf[1024];
	int bytes = 0;
	switch (tok->token) {
	case T_NAME:
		snprintf(buf, sizeof(buf), "name %s", tok->name);
		return buf;
	case T_LITERAL:
		switch (tok->storage) {
		case STORAGE_U8:
		case STORAGE_U16:
		case STORAGE_U32:
		case STORAGE_U64:
		case STORAGE_UINT:
		case STORAGE_UINTPTR:
		case STORAGE_SIZE:
			snprintf(buf, sizeof(buf), "%ju", tok->uval);
			break;
		case STORAGE_I8:
		case STORAGE_I16:
		case STORAGE_I32:
		case STORAGE_I64:
		case STORAGE_ICONST:
		case STORAGE_INT:
			snprintf(buf, sizeof(buf), "%jd", tok->ival);
			break;
		case STORAGE_F32:
		case STORAGE_F64:
		case STORAGE_FCONST:
			snprintf(buf, sizeof(buf), "%lf", tok->fval);
			break;
		case STORAGE_RCONST:
			bytes += snprintf(&buf[bytes], sizeof(buf) - bytes, "'");
			bytes += snprintf(&buf[bytes], sizeof(buf) - bytes, "%s",
				rune_unparse(tok->rune));
			bytes += snprintf(&buf[bytes], sizeof(buf) - bytes, "'");
			break;
		case STORAGE_STRING:
			return string_unparse(tok);
		case STORAGE_ALIAS:
		case STORAGE_ARRAY:
		case STORAGE_BOOL:
		case STORAGE_CHAR:
		case STORAGE_ENUM:
		case STORAGE_ERROR:
		case STORAGE_FUNCTION:
		case STORAGE_POINTER:
		case STORAGE_NULL:
		case STORAGE_RUNE:
		case STORAGE_SLICE:
		case STORAGE_STRUCT:
		case STORAGE_TAGGED:
		case STORAGE_TUPLE:
		case STORAGE_UNION:
		case STORAGE_VALIST:
		case STORAGE_VOID:
			assert(0);
		}
		return buf;
	default:;
		const char *out = lexical_token_str(tok->token);
		return out;
	}
}

void
unlex(struct lexer *lexer, struct token *in)
{
	assert(lexer->un.token == T_ERROR && "Only one unlex is supported");
	lexer->un = *in;
}
