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
	// Must match enum lexical_token (lex.h)
	[T_ATTR_FINI] = "@fini",
	[T_ATTR_INIT] = "@init",
	[T_ATTR_OFFSET] = "@offset",
	[T_ATTR_PACKED] = "@packed",
	[T_ATTR_SYMBOL] = "@symbol",
	[T_ATTR_TEST] = "@test",
	[T_ATTR_THREADLOCAL] = "@threadlocal",
	[T_UNDERSCORE] = "_",
	[T_ABORT] = "abort",
	[T_ALIGN] = "align",
	[T_ALLOC] = "alloc",
	[T_APPEND] = "append",
	[T_AS] = "as",
	[T_ASSERT] = "assert",
	[T_BOOL] = "bool",
	[T_BREAK] = "break",
	[T_CASE] = "case",
	[T_CONST] = "const",
	[T_CONTINUE] = "continue",
	[T_DEFER] = "defer",
	[T_DEF] = "def",
	[T_DELETE] = "delete",
	[T_DONE] = "done",
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
	[T_NEVER] = "never",
	[T_NULL] = "null",
	[T_NULLABLE] = "nullable",
	[T_OFFSET] = "offset",
	[T_OPAQUE] = "opaque",
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
	[T_DOUBLE_DOT] = "..",
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
	[T_TIMES] = "*",
	[T_TIMESEQ] = "*=",
	[T_BXOR] = "^",
	[T_BXOREQ] = "^=",
};

static_assert(sizeof(tokens) / sizeof(const char *) == T_LAST_OPERATOR + 1,
	"tokens array isn't in sync with lexical_token enum");

static noreturn void
error(struct location loc, const char *fmt, ...)
{
	xfprintf(stderr, "%s:%d:%d: syntax error: ", sources[loc.file],
			loc.lineno, loc.colno);

	va_list ap;
	va_start(ap, fmt);
	xvfprintf(stderr, fmt, ap);
	va_end(ap);

	xfprintf(stderr, "\n");
	errline(loc);
	exit(EXIT_LEX);
}

void
lex_init(struct lexer *lexer, FILE *f, int fileid)
{
	memset(lexer, 0, sizeof(*lexer));
	lexer->in = f;
	lexer->bufsz = 256;
	lexer->buf = xcalloc(1, lexer->bufsz);
	lexer->un.token = T_NONE;
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

static void
append_buffer(struct lexer *lexer, const char *buf, size_t sz)
{
	if (lexer->buflen + sz >= lexer->bufsz) {
		lexer->bufsz *= 2;
		lexer->buf = xrealloc(lexer->buf, lexer->bufsz);
	}
	memcpy(lexer->buf + lexer->buflen, buf, sz);
	lexer->buflen += sz;
	lexer->buf[lexer->buflen] = '\0';
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
			error(lexer->loc, "Invalid UTF-8 sequence encountered");
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
	char buf[UTF8_MAX_SIZE];
	size_t sz = utf8_encode(&buf[0], c);
	append_buffer(lexer, buf, sz);
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
clearbuf(struct lexer *lexer) {
	lexer->buflen = 0;
	lexer->buf[0] = 0;
}

static void
consume(struct lexer *lexer, size_t n)
{
	for (size_t i = 0; i < n; i++) {
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

static enum lexical_token
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
			error(out->loc, "Unknown attribute %s", lexer->buf);
		}
		out->token = T_NAME;
		out->name = xstrdup(lexer->buf);
	} else {
		out->token = (const char **)token - tokens;
	}
	clearbuf(lexer);
	return out->token;
}

static uint64_t
compute_exp(uint64_t n, int exponent, bool _signed)
{
	if (n == 0) {
		return 0;
	}
	for (int i = 0; i < exponent; i++) {
		uint64_t old = n;
		n *= 10;
		if (n / 10 != old) {
			errno = ERANGE;
			return INT64_MAX;
		}
	}
	if (_signed && n > (uint64_t)INT64_MIN) {
		errno = ERANGE;
		return INT64_MAX;
	}
	return n;
}

static void
lex_number(struct lexer *lexer, struct token *out)
{
	enum bases {
		BIN = 1, OCT, HEX, DEC = 0x07, MASK = DEC
	};
	static_assert((BIN | OCT | HEX | DEC) == DEC, "DEC bits must be a superset of all other bases");
	enum flags {
		FLT = 3, EXP, SUFF, DIG,
	};

	static const char chrs[][24] = {
		[BIN] = "01",
		[OCT] = "01234567",
		[DEC] = "0123456789",
		[HEX] = "0123456789abcdefABCDEF",
	};

	static const char matching_states[0x80][6] = {
		['.'] = {DEC, HEX, 0},
		['e'] = {DEC, DEC | 1<<FLT, 0},
		['E'] = {DEC, DEC | 1<<FLT, 0},
		['p'] = {HEX, HEX | 1<<FLT, 0},
		['P'] = {HEX, HEX | 1<<FLT, 0},
		['+'] = {DEC | 1<<EXP | 1<<DIG, DEC | 1<<FLT | 1<<EXP | 1<<DIG, 0},
		['-'] = {DEC | 1<<EXP | 1<<DIG, DEC | 1<<FLT | 1<<EXP | 1<<DIG, 0},
		['i'] = {BIN, OCT, HEX, DEC, DEC | 1<<EXP, 0},
		['u'] = {BIN, OCT, HEX, DEC, DEC | 1<<EXP, 0},
		['z'] = {BIN, OCT, HEX, DEC, DEC | 1<<EXP, 0},
		['f'] = {DEC, DEC | 1<<FLT, DEC | 1<<EXP, DEC | 1<<FLT | 1<<EXP, 0},
	};
	int state = DEC, base = 10, oldstate = DEC;
	uint32_t c = next(lexer, &out->loc, true), last = 0;
	assert(c != C_EOF && c <= 0x7F && isdigit(c));
	if (c == '0') {
		c = next(lexer, NULL, true);
		if (c <= 0x7F && isdigit(c)) {
			error(out->loc, "Leading zero in base 10 literal");
		} else if (c == 'b') {
			state = BIN | 1 << DIG;
			base = 2;
		} else if (c == 'o') {
			state = OCT | 1 << DIG;
			base = 8;
		} else if (c == 'x') {
			state = HEX | 1 << DIG;
			base = 16;
		}
	}
	if (state != DEC) {
		last = c;
		c = next(lexer, NULL, true);
	}
	size_t exp = 0, suff = 0;
	do {
		if (strchr(chrs[state & MASK], c)) {
			state &= ~(1 << DIG);
			last = c;
			continue;
		} else if (c > 0x7f || !strchr(matching_states[c], state)) {
			goto end;
		}
		oldstate = state;
		switch (c) {
		case '.':
			if (lexer->require_int) {
				goto want_int;
			}
			state |= 1 << FLT;
			break;
		case '-':
		case 'p':
		case 'P':
			state |= 1 << FLT;
			/* fallthrough */
		case 'e':
		case 'E':
		case '+':
			state |= DEC | 1 << EXP;
			exp = lexer->buflen - 1;
			break;
		case 'f':
			state |= 1 << FLT;
			/* fallthrough */
		case 'i':
		case 'u':
		case 'z':
			state |= DEC | 1 << SUFF;
			suff = lexer->buflen - 1;
			break;
		default:
			goto end;
		}
		if (state & 1 << FLT && lexer->require_int) {
			error(out->loc, "Expected integer literal");
		}
		last = c;
		state |= 1 << DIG;
	} while ((c = next(lexer, NULL, true)) != C_EOF);
	last = 0;
end:
	if (last && !strchr("iuz", last) && !strchr(chrs[state & MASK], last)) {
		state = oldstate;
		push(lexer, c, true);
		push(lexer, last, true);
	} else if (c != C_EOF) {
want_int:
		push(lexer, c, true);
	}
	out->token = T_NUMBER;
	lexer->require_int = false;

	enum kind {
		UNKNOWN = -1,
		ICONST, SIGNED, UNSIGNED, FLOAT
	} kind = UNKNOWN;
	static const struct {
		const char suff[4];
		enum kind kind;
		enum type_storage storage;
	} storages[] = {
		{"f32", FLOAT, STORAGE_F32},
		{"f64", FLOAT, STORAGE_F64},
		{"i", SIGNED, STORAGE_INT},
		{"i16", SIGNED, STORAGE_I16},
		{"i32", SIGNED, STORAGE_I32},
		{"i64", SIGNED, STORAGE_I64},
		{"i8", SIGNED, STORAGE_I8},
		{"u", UNSIGNED, STORAGE_UINT},
		{"u16", UNSIGNED, STORAGE_U16},
		{"u32", UNSIGNED, STORAGE_U32},
		{"u64", UNSIGNED, STORAGE_U64},
		{"u8", UNSIGNED, STORAGE_U8},
		{"z", UNSIGNED, STORAGE_SIZE},
	};
	if (suff) {
		for (size_t i = 0; i < sizeof storages / sizeof storages[0]; i++) {
			if (!strcmp(storages[i].suff, lexer->buf + suff)) {
				out->storage = storages[i].storage;
				kind = storages[i].kind;
				break;
			}
		}
		if (kind == UNKNOWN) {
			error(out->loc, "Invalid suffix '%s'", lexer->buf + suff);
		}
	}
	if (state & 1 << FLT) {
		if (kind == UNKNOWN) {
			out->storage = STORAGE_FCONST;
		} else if (kind != FLOAT) {
			error(out->loc, "Unexpected decimal point in integer literal");
		}
		out->fval = strtod(lexer->buf, NULL);
		clearbuf(lexer);
		return;
	}

	if (kind == UNKNOWN) {
		kind = ICONST;
		out->storage = STORAGE_ICONST;
	}
	uint64_t exponent = 0;
	errno = 0;
	if (exp != 0) {
		exponent = strtoumax(lexer->buf + exp + 1, NULL, 10);
	}
	out->uval = strtoumax(lexer->buf + (base == 10 ? 0 : 2), NULL, base);
	out->uval = compute_exp(out->uval, exponent, kind == SIGNED);
	if (errno == ERANGE) {
		error(out->loc, "Integer literal overflow");
	}
	if (kind == ICONST && out->uval > (uint64_t)INT64_MAX) {
		out->storage = STORAGE_U64;
	} else if (kind == SIGNED && out->uval == (uint64_t)INT64_MIN) {
		// XXX: Hack
		out->ival = INT64_MIN;
	} else if (kind != UNSIGNED) {
		out->ival = (int64_t)out->uval;
	}
	clearbuf(lexer);
}

static size_t
lex_rune(struct lexer *lexer, char *out)
{
	char buf[9];
	char *endptr;
	struct location loc;
	uint32_t c = next(lexer, NULL, false);
	assert(c != C_EOF);

	switch (c) {
	case '\\':
		loc = lexer->loc;
		c = next(lexer, NULL, false);
		switch (c) {
		case '0':
			out[0] = '\0';
			return 1;
		case 'a':
			out[0] = '\a';
			return 1;
		case 'b':
			out[0] = '\b';
			return 1;
		case 'f':
			out[0] = '\f';
			return 1;
		case 'n':
			out[0] = '\n';
			return 1;
		case 'r':
			out[0] = '\r';
			return 1;
		case 't':
			out[0] = '\t';
			return 1;
		case 'v':
			out[0] = '\v';
			return 1;
		case '\\':
			out[0] = '\\';
			return 1;
		case '\'':
			out[0] = '\'';
			return 1;
		case '"':
			out[0] = '\"';
			return 1;
		case 'x':
			buf[0] = next(lexer, NULL, false);
			buf[1] = next(lexer, NULL, false);
			buf[2] = '\0';
			c = strtoul(&buf[0], &endptr, 16);
			if (*endptr != '\0') {
				error(loc, "Invalid hex literal");
			}
			out[0] = c;
			return 1;
		case 'u':
			buf[0] = next(lexer, NULL, false);
			buf[1] = next(lexer, NULL, false);
			buf[2] = next(lexer, NULL, false);
			buf[3] = next(lexer, NULL, false);
			buf[4] = '\0';
			c = strtoul(&buf[0], &endptr, 16);
			if (*endptr != '\0') {
				error(loc, "Invalid hex literal");
			}
			return utf8_encode(out, c);
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
				error(loc, "Invalid hex literal");
			}
			return utf8_encode(out, c);
		case C_EOF:
			error(lexer->loc, "Unexpected end of file");
		default:
			error(loc, "Invalid escape '\\%c'", c);
		}
		assert(0);
	default:
		return utf8_encode(out, c);
	}
	assert(0);
}

static enum lexical_token
lex_string(struct lexer *lexer, struct token *out)
{
	uint32_t c = next(lexer, &out->loc, false);
	uint32_t delim;
	char buf[UTF8_MAX_SIZE + 1];

	switch (c) {
	case '"':
	case '`':
		delim = c;
		while ((c = next(lexer, NULL, false)) != delim) {
			if (c == C_EOF) {
				error(lexer->loc, "Unexpected end of file");
			}
			push(lexer, c, false);
			if (delim == '"') {
				size_t sz = lex_rune(lexer, buf);
				append_buffer(lexer, buf, sz);
			} else {
				next(lexer, NULL, true);
			}
		}
		char *s = xcalloc(lexer->buflen + 1, 1);
		memcpy(s, lexer->buf, lexer->buflen);
		out->token = T_NUMBER;
		out->storage = STORAGE_STRING;
		out->string.len = lexer->buflen;
		out->string.value = s;
		clearbuf(lexer);
		return out->token;
	case '\'':
		c = next(lexer, NULL, false);
		switch (c) {
		case '\'':
			error(out->loc, "Expected rune before trailing single quote");
		case '\\':
			push(lexer, c, false);
			struct location loc = lexer->loc;
			size_t sz = lex_rune(lexer, buf);
			buf[sz] = '\0';
			const char *s = buf;
			out->rune = utf8_decode(&s);
			if (out->rune == UTF8_INVALID) {
				error(loc, "invalid UTF-8 in rune literal");
			}
			break;
		default:
			out->rune = c;
		}
		if (next(lexer, NULL, false) != '\'') {
			error(out->loc, "Expected trailing single quote");
		}
		out->token = T_NUMBER;
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
				out->token = T_DOUBLE_DOT;
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
			return lex(lexer, out);
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
			buf[utf8_encode(buf, c)] = '\0';
		}
		break;
	}
	return buf;
}

enum lexical_token
lex(struct lexer *lexer, struct token *out)
{
	if (lexer->un.token != T_NONE) {
		*out = lexer->un;
		lexer->un.token = T_NONE;
		return out->token;
	}

	uint32_t c = wgetc(lexer, &out->loc);
	if (c == C_EOF) {
		out->token = T_EOF;
		return out->token;
	}

	if (c <= 0x7F && isdigit(c)) {
		push(lexer, c, false);
		lex_number(lexer, out);
		return T_NUMBER;
	}

	lexer->require_int = false;

	if (c <= 0x7F && (isalpha(c) || c == '_' || c == '@')) {
		push(lexer, c, false);
		return lex_name(lexer, out);
	}

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
		error(lexer->loc, "unexpected codepoint '%s'", rune_unparse(c));
	}

	return out->token;
}

void
token_finish(struct token *tok)
{
	switch (tok->token) {
	case T_NAME:
		free(tok->name);
		break;
	case T_NUMBER:
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
	case T_NUMBER:
		return "number";
	case T_EOF:
		return "end of file";
	case T_NONE:
		abort();
	default:
		assert(tok < sizeof(tokens) / sizeof(tokens[0]));
		return tokens[tok];
	}
}

static const char *
string_unparse(const struct token *tok)
{
	static char buf[1024];
	assert(tok->token == T_NUMBER && tok->storage == STORAGE_STRING);
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
	case T_NUMBER:
		switch (tok->storage) {
		case STORAGE_U8:
		case STORAGE_U16:
		case STORAGE_U32:
		case STORAGE_U64:
		case STORAGE_UINT:
		case STORAGE_UINTPTR:
		case STORAGE_SIZE:
			snprintf(buf, sizeof(buf), "%" PRIu64, tok->uval);
			break;
		case STORAGE_I8:
		case STORAGE_I16:
		case STORAGE_I32:
		case STORAGE_I64:
		case STORAGE_ICONST:
		case STORAGE_INT:
			snprintf(buf, sizeof(buf), "%" PRIi64, tok->ival);
			break;
		case STORAGE_F32:
		case STORAGE_F64:
		case STORAGE_FCONST:
			snprintf(buf, sizeof(buf), "%f", tok->fval);
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
		case STORAGE_ENUM:
		case STORAGE_ERROR:
		case STORAGE_FUNCTION:
		case STORAGE_POINTER:
		case STORAGE_NEVER:
		case STORAGE_NULL:
		case STORAGE_OPAQUE:
		case STORAGE_RUNE:
		case STORAGE_SLICE:
		case STORAGE_STRUCT:
		case STORAGE_TAGGED:
		case STORAGE_TUPLE:
		case STORAGE_UNION:
		case STORAGE_VALIST:
		case STORAGE_VOID:
		case STORAGE_DONE:
			assert(0);
		}
		return buf;
	default:;
		const char *out = lexical_token_str(tok->token);
		return out;
	}
}

void
unlex(struct lexer *lexer, const struct token *in)
{
	assert(lexer->un.token == T_NONE);
	lexer->un = *in;
}
