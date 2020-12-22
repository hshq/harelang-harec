#ifndef HAREC_EXPR_H
#define HAREC_EXPR_H
#include <stdint.h>
#include "identifier.h"
#include "types.h"

struct scope;
struct scope_object;

enum expr_type {
	EXPR_ACCESS,
	EXPR_ASSERT,
	EXPR_ASSIGN,
	EXPR_BINARITHM,
	EXPR_BINDING,
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

struct expression_access {
	const struct scope_object *object;
};

struct expression_binding {
	const struct scope_object *object;
	struct expression *initializer;
	struct expression_binding *next;
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

struct expressions {
	struct expression *expr;
	struct expressions *next;
};

struct expression_list {
	struct scope *scope;
	struct expressions exprs;
};

struct expression_return {
	struct expression *value;
};

struct expression {
	const struct type *result;
	enum expr_type type;
	bool terminates;
	union {
		struct expression_access access;
		struct expression_binding binding;
		union expression_constant constant;
		struct expression_list list;
		struct expression_return _return;
	};
};

#endif
