#ifndef HAREC_LEX_H
#define HAREC_LEX_H
#include <stdint.h>
#include <stdio.h>
#include "types.h"
#include "utf8.h"

#define C_EOF UTF8_INVALID

// Keep sorted
enum lexical_token {
	T_ATTR_FINI,
	T_ATTR_INIT,
	T_ATTR_NORETURN,
	T_ATTR_OFFSET,
	T_ATTR_PACKED,
	T_ATTR_SYMBOL,
	T_ATTR_TEST,
	T_ATTR_THREADLOCAL,
	T_UNDERSCORE,
	T_ABORT,
	T_ALIGN,
	T_ALLOC,
	T_APPEND,
	T_AS,
	T_ASSERT,
	T_BOOL,
	T_BREAK,
	T_CAP,
	T_CASE,
	T_CONST,
	T_CONTINUE,
	T_DEF,
	T_DEFER,
	T_DELETE,
	T_ELSE,
	T_ENUM,
	T_EXPORT,
	T_F32,
	T_F64,
	T_FALSE,
	T_FN,
	T_FOR,
	T_FREE,
	T_I16,
	T_I32,
	T_I64,
	T_I8,
	T_IF,
	T_INSERT,
	T_INT,
	T_IS,
	T_LEN,
	T_LET,
	T_MATCH,
	T_NULL,
	T_NULLABLE,
	T_OFFSET,
	T_RETURN,
	T_RUNE,
	T_SIZE,
	T_STATIC,
	T_STR,
	T_STRUCT,
	T_SWITCH,
	T_TRUE,
	T_TYPE,
	T_U16,
	T_U32,
	T_U64,
	T_U8,
	T_UINT,
	T_UINTPTR,
	T_UNION,
	T_USE,
	T_VAARG,
	T_VAEND,
	T_VALIST,
	T_VASTART,
	T_VOID,
	T_YIELD,
	T_LAST_KEYWORD = T_YIELD,

	// Operators
	T_ARROW,
	T_BANDEQ,
	T_BAND,
	T_BNOT,
	T_BOR,
	T_COLON,
	T_COMMA,
	T_DIV,
	T_DIVEQ,
	T_DOT,
	T_DOUBLE_COLON,
	T_ELLIPSIS,
	T_EQUAL,
	T_GREATER,
	T_GREATEREQ,
	T_LAND,
	T_LANDEQ,
	T_LBRACE,
	T_LBRACKET,
	T_LEQUAL,
	T_LESS,
	T_LESSEQ,
	T_LNOT,
	T_LOR,
	T_LOREQ,
	T_LPAREN,
	T_LSHIFT,
	T_LSHIFTEQ,
	T_LXOR,
	T_LXOREQ,
	T_MINUS,
	T_MINUSEQ,
	T_MODEQ,
	T_MODULO,
	T_NEQUAL,
	T_BOREQ,
	T_PLUS,
	T_PLUSEQ,
	T_QUESTION,
	T_RBRACE,
	T_RBRACKET,
	T_RPAREN,
	T_RSHIFT,
	T_RSHIFTEQ,
	T_SEMICOLON,
	T_SLICE,
	T_TIMES,
	T_TIMESEQ,
	T_BXOR,
	T_BXOREQ,
	T_LAST_OPERATOR = T_BXOREQ,

	// Tokens with additional information
	T_LITERAL,
	T_NAME,

	// Magic tokens
	T_EOF,
	T_NONE,
};

struct location {
	int file;
	int lineno, colno;
};

struct token {
	struct location loc;
	enum lexical_token token;
	enum type_storage storage;
	union {
		char *name;
		uint32_t rune;
		int64_t ival;
		uint64_t uval;
		double fval;
		struct {
			size_t len;
			char *value;
		} string;
	};
};

struct lexer {
	FILE *in;
	char *buf;
	size_t bufsz, buflen;
	uint32_t c[2];
	struct token un;
	struct location loc;
	bool require_int;
};

void lex_init(struct lexer *lexer, FILE *f, int fileid);
void lex_finish(struct lexer *lexer);
enum lexical_token lex(struct lexer *lexer, struct token *out);
void unlex(struct lexer *lexer, struct token *in);

void token_finish(struct token *tok);
const char *token_str(const struct token *tok);
const char *lexical_token_str(enum lexical_token tok);

#endif
