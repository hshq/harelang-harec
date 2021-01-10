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
	EXPR_IF,
	EXPR_LIST,
	EXPR_MATCH,
	EXPR_MEASURE,
	EXPR_RETURN,
	EXPR_SLICE,
	EXPR_STRUCT,
	EXPR_SWITCH,
	EXPR_UNARITHM,
};

enum access_type {
	ACCESS_IDENTIFIER,
	ACCESS_INDEX,
	ACCESS_FIELD,
};

struct expression_access {
	enum access_type type;
	union {
		const struct scope_object *object;
		struct {
			struct expression *array;
			struct expression *index;
		};
		struct {
			struct expression *_struct;
			const struct struct_field *field;
		};
	};
};

struct expression_assert {
	struct expression *cond;
	struct expression *message;
	bool is_static;
};

enum binarithm_operator {
	BIN_BAND,	// &
	BIN_BOR,	// |
	BIN_DIV,	// /
	BIN_GREATER,	// >
	BIN_GREATEREQ,	// >=
	BIN_LAND,	// &&
	BIN_LEQUAL,	// ==
	BIN_LESS,	// <
	BIN_LESSEQ,	// <=
	BIN_LOR,	// ||
	BIN_LSHIFT,	// <<
	BIN_LXOR,	// ^^
	BIN_MINUS,	// -
	BIN_MODULO,	// %
	BIN_NEQUAL,	// !=
	BIN_PLUS,	// +
	BIN_RSHIFT,	// >>
	BIN_TIMES,	// *
	BIN_BXOR,	// ^
};

struct expression_assign {
	enum binarithm_operator op;
	struct expression *object, *value;
	bool indirect;
};

struct expression_binarithm {
	enum binarithm_operator op;
	struct expression *lvalue, *rvalue;
};

struct expression_binding {
	const struct scope_object *object;
	struct expression *initializer;
	struct expression_binding *next;
};

enum cast_kind {
	C_CAST,
	C_ASSERTION,
	C_TEST,
};

struct expression_cast {
	enum cast_kind kind;
	struct expression *value;
};

struct call_argument {
	bool variadic;
	struct expression *value;
	struct call_argument *next;
};

struct expression_call {
	struct expression *lvalue;
	struct call_argument *args;
};

struct array_constant {
	struct expression *value;
	struct array_constant *next;
	bool expand;
};

union expression_constant {
	bool bval;
	double fval;
	intmax_t ival;
	uintmax_t uval;
	uint32_t rune;
	struct {
		size_t len;
		char *value;
	} string;
	struct array_constant *array;
	// TODO: Struct constants
};

struct expression_for {
	struct scope *scope;
	struct expression *bindings;
	struct expression *cond;
	struct expression *afterthought;
	struct expression *body;
};

struct expression_if {
	struct expression *cond;
	struct expression *true_branch, *false_branch;
};

struct expressions {
	struct expression *expr;
	struct expressions *next;
};

struct expression_list {
	struct scope *scope;
	struct expressions exprs;
};

enum measure_operator {
	M_LEN,
	M_SIZE,
	M_OFFSET,
};

struct expression_measure {
	enum measure_operator op;
	union {
		struct expression *value;
		const struct type *type;
		// TODO: Field selection
	};
};

struct expression_return {
	struct expression *value;
};

struct expression_struct {
	const struct struct_field *field;
	struct expression *value;
	struct expression_struct *next;
};

enum unarithm_operator {
	UN_ADDRESS,	// &
	UN_BNOT,	// ~
	UN_DEREF,	// *
	UN_LNOT,	// !
	UN_MINUS,	// -
	UN_PLUS,	// +
};

struct expression_unarithm {
	enum unarithm_operator op;
	struct expression *operand;
};

struct expression {
	const struct type *result;
	enum expr_type type;
	bool terminates;
	union {
		struct expression_access access;
		struct expression_assert assert;
		struct expression_assign assign;
		struct expression_binarithm binarithm;
		struct expression_binding binding;
		struct expression_call call;
		struct expression_cast cast;
		union expression_constant constant;
		struct expression_for _for;
		struct expression_if _if;
		struct expression_list list;
		struct expression_measure measure;
		struct expression_return _return;
		struct expression_struct _struct;
		struct expression_unarithm unarithm;
	};
};

#endif
