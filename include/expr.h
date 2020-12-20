#ifndef HAREC_EXPR_H
#define HAREC_EXPR_H
#include <stdint.h>
#include "types.h"

enum expr_type {
	EXPR_ACCESS,
	EXPR_ASSERT,
	EXPR_ASSIGN,
	EXPR_BINARITHM,
	EXPR_BINDING_LIST,
	EXPR_BREAK,
	EXPR_CALL,
	EXPR_CAST,
	EXPR_CONSTANT,
	EXPR_CONTINUE,
	EXPR_FOR,
	EXPR_FREE,
	EXPR_IF,
	EXPR_INDEX,
	EXPR_LIST,
	EXPR_MATCH,
	EXPR_MEASURE,
	EXPR_RETURN,
	EXPR_SLICE,
	EXPR_STRUCT,
	EXPR_SWITCH,
	EXPR_UNARITHM,
	EXPR_WHILE,
};

// TODO: Stretchy constants
union expression_constant {
	bool bval;
	struct {
		char *sval;
		size_t ssz;
	};
	double fval;
	intmax_t ival;
	uintmax_t uval;
	uint32_t rune;
	// TODO: Array, slice, struct constants
};

struct expression_list {
	struct expression *expr;
	struct expression_list *next;
};

struct expression_return {
	struct expression *value;
};

struct expression {
	const struct type *result;
	enum expr_type type;
	bool terminates;
	union {
		union expression_constant constant;
		struct expression_list list;
		struct expression_return _return;
	};
};

#endif
