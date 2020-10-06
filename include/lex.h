#ifndef HAREC_LEX_H
#define HAREC_LEX_H

// Keep sorted
enum lexical_token {
	T_AS,
	T_ASSERT,
	T_BOOL,
	T_BREAK,
	T_CHAR,
	T_CONST,
	T_CONTINUE,
	T_DEF,
	T_ELSE,
	T_ENUM,
	T_EXPORT,
	T_F32,
	T_F64,
	T_FALSE,
	T_FN,
	T_FOR,
	T_I16,
	T_I32,
	T_I64,
	T_I8,
	T_IF,
	T_INT,
	T_IS,
	T_LEN,
	T_LET,
	T_MATCH,
	T_NULL,
	T_NULLABLE,
	T_RETURN,
	T_SIZE,
	T_STATIC,
	T_STR,
	T_STRUCT,
	T_SWITCH,
	T_TRUE,
	T_U16,
	T_U32,
	T_U64,
	T_U8,
	T_UINT,
	T_UINTPTR,
	T_UNION,
	T_USE,
	T_VOID,
	T_WHILE,

	// Operators
	T_ANDEQ,
	T_BAND,
	T_BNOT,
	T_BOR,
	T_CASE,
	T_COLON,
	T_COMMA,
	T_DIV,
	T_DIVEQ,
	T_DOT,
	T_DOUBLE_COLON,
	T_ELLIPSIS,
	T_EQUAL,
	T_GREATER,
	T_GTR_EQL,
	T_LAND,
	T_LBRACE,
	T_LBRACKET,
	T_LEQUAL,
	T_LESS,
	T_LESS_EQL,
	T_LNOT,
	T_LOR,
	T_LPAREN,
	T_LSHIFT,
	T_LSHIFTEQ,
	T_MINUS,
	T_MINUSEQ,
	T_MODEQ,
	T_MODULO,
	T_NEQUAL,
	T_OREQ,
	T_PLUS,
	T_PLUSEQ,
	T_RBRACE,
	T_RBRACKET,
	T_RPAREN,
	T_RSHIFT,
	T_RSHIFTEQ,
	T_SEMICOLON,
	T_SLICE,
	T_TIMES,
	T_TIMESEQ,
	T_XOREQ,

	// Tokens with additional information
	T_NAME,
	T_LITERAL,

	// Magic tokens
	T_EOF,
	T_ERROR,
};

struct token {
	enum lexical_token token;
	union {
		char *name;
	};
};

struct lexer {
	FILE *in;
	char *buf;
	size_t bufsz, buflen;
};

void lex_init(struct lexer *lexer, FILE *f);
int lex(struct lexer *lexer, struct token *out);

void token_finish(struct token *tok);
const char *token_str(const struct token *tok);

#endif
