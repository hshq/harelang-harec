#ifndef HAREC_EXPR_H
#define HAREC_EXPR_H
#include <stdint.h>
#include "identifier.h"
#include "lex.h"
#include "types.h"

struct scope;
struct scope_object;

enum expr_type {
	EXPR_ACCESS,
	EXPR_ALLOC,
	EXPR_APPEND,
	EXPR_ASSERT,
	EXPR_ASSIGN,
	EXPR_BINARITHM,
	EXPR_BINDING,
	EXPR_BREAK,
	EXPR_CALL,
	EXPR_CAST,
	EXPR_COMPOUND,
	EXPR_CONSTANT,
	EXPR_CONTINUE,
	EXPR_DEFER,
	EXPR_DEFINE,
	EXPR_DELETE,
	EXPR_FOR,
	EXPR_FREE,
	EXPR_IF,
	EXPR_INSERT,
	EXPR_MATCH,
	EXPR_MEASURE,
	EXPR_PROPAGATE,
	EXPR_RETURN,
	EXPR_SLICE,
	EXPR_STRUCT,
	EXPR_SWITCH,
	EXPR_TUPLE,
	EXPR_UNARITHM,
	EXPR_VAARG,
	EXPR_VAEND,
	EXPR_VASTART,
	EXPR_YIELD,
};

struct expressions {
	struct expression *expr;
	struct expressions *next;
};

enum access_type {
	ACCESS_IDENTIFIER,
	ACCESS_INDEX,
	ACCESS_FIELD,
	ACCESS_TUPLE,
};

struct expression_access {
	enum access_type type;
	union {
		const struct scope_object *object;
		struct {
			struct expression *array;
			struct expression *index;
			bool bounds_checked;
		};
		struct {
			struct expression *_struct;
			const struct struct_field *field;
		};
		struct {
			struct expression *tuple;
			const struct type_tuple *tvalue;
			size_t tindex;
		};
	};
};

enum alloc_kind {
	ALLOC_OBJECT,	// alloc(42)
	ALLOC_WITH_CAP,	// alloc([], 42)
	ALLOC_WITH_LEN,	// alloc([0...], 42)
	ALLOC_COPY,	// alloc(x...);
};

struct expression_alloc {
	enum alloc_kind kind;
	struct expression *init;
	struct expression *cap;
};

struct expression_append {
	struct expression *object;
	struct expression *value;
	struct expression *length;
	bool is_static, is_multi;
};

enum fixed_aborts {
	ABORT_OOB = 0,
	ABORT_TYPE_ASSERTION = 1,
	ABORT_ALLOC_FAILURE = 2,
	ABORT_STATIC_EXCEEDED = 3,
	ABORT_UNREACHABLE = 4,
	ABORT_CAP_TOO_SMALL = 5,
	ABORT_ANON_ASSERTION_FAILED = 6,
	ABORT_PROPAGATE_ERROR_OCCURRED = 7,
};

struct expression_assert {
	struct expression *cond;
	struct expression *message;
	enum fixed_aborts fixed_reason;
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
	BIN_LAST = BIN_BXOR,
};

struct expression_assign {
	enum binarithm_operator op;
	struct expression *object, *value;
};

struct expression_binarithm {
	enum binarithm_operator op;
	struct expression *lvalue, *rvalue;
};

struct binding_unpack {
	const struct scope_object *object;
	size_t offset;
	struct binding_unpack *next;
};

struct expression_binding {
	const struct scope_object *object;
	struct binding_unpack *unpack;
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
	const struct type *secondary;
	struct expression *value;
	bool lowered;
};

struct call_argument {
	struct expression *value;
	struct call_argument *next;
};

struct expression_call {
	struct expression *lvalue;
	struct call_argument *args;
};

struct expression_compound {
	char *label;
	struct scope *scope;
	struct expressions exprs;
};

struct array_constant {
	struct expression *value;
	struct array_constant *next;
};

// Invariant: these are sorted by field offset
struct struct_constant {
	const struct struct_field *field;
	struct expression *value;
	struct struct_constant *next;
};

struct tuple_constant {
	const struct type_tuple *field;
	struct expression *value;
	struct tuple_constant *next;
};

struct tagged_constant {
	const struct type *tag;
	struct expression *value;
};

struct expression_constant {
	// If non-null, ival is an offset from this object's address
	const struct scope_object *object;
	union {
		bool bval;
		double fval;
		int64_t ival;
		uint64_t uval;
		uint32_t rune;
		struct {
			size_t len;
			char *value;
		} string;
		struct array_constant *array;
		struct struct_constant *_struct;
		struct tuple_constant *tuple;
		struct tagged_constant tagged;
	};
};

struct expression_control {
	char *label;
	const struct scope *scope;
	struct expression *value; // Only set for yield
};

struct expression_defer {
	struct scope *scope;
	struct expression *deferred;
};

struct expression_delete {
	struct expression *expr;
	bool is_static;
};

struct expression_for {
	struct scope *scope;
	struct expression *bindings;
	struct expression *cond;
	struct expression *afterthought;
	struct expression *body;
};

struct expression_free {
	struct expression *expr;
};

struct expression_if {
	struct expression *cond;
	struct expression *true_branch, *false_branch;
};

struct match_case {
	const struct scope_object *object;	// NULL if not bound
	const struct type *type;		// NULL if default
	struct expression *value;
	struct match_case *next;
};

struct expression_match {
	struct expression *value;
	struct match_case *cases;
};

enum measure_operator {
	M_ALIGN,
	M_LEN,
	M_SIZE,
	M_OFFSET,
};

struct expression_measure {
	enum measure_operator op;
	union {
		struct expression *value;
		struct dimensions dimensions;
		// TODO: Field selection
	};
};

struct expression_return {
	struct expression *value;
};

struct expression_slice {
	struct expression *object;
	struct expression *start, *end;
	bool bounds_checked;
};

struct case_option {
	struct expression *value;
	struct case_option *next;
};

struct switch_case {
	struct case_option *options; // NULL for default case
	struct expression *value;
	struct switch_case *next;
};

struct expression_switch {
	struct expression *value;
	struct switch_case *cases;
};

struct expr_struct_field {
	const struct struct_field *field;
	struct expression *value;
	struct expr_struct_field *next;
};

struct expression_struct {
	struct expr_struct_field *fields;
	bool autofill;
};

struct expression_tuple {
	struct expression *value;
	struct expression_tuple *next;
};

enum unarithm_operator {
	UN_ADDRESS,	// &
	UN_BNOT,	// ~
	UN_DEREF,	// *
	UN_LNOT,	// !
	UN_MINUS,	// -
};

struct expression_unarithm {
	enum unarithm_operator op;
	struct expression *operand;
};

struct expression_vaarg {
	struct expression *ap;
};

struct expression {
	const struct type *result;
	enum expr_type type;
	struct location loc; // For fixed aborts
	union {
		struct expression_access access;
		struct expression_alloc alloc;
		struct expression_append append; // and insert
		struct expression_assert assert;
		struct expression_assign assign;
		struct expression_binarithm binarithm;
		struct expression_binding binding;
		struct expression_call call;
		struct expression_cast cast;
		struct expression_compound compound;
		struct expression_constant constant;
		struct expression_defer defer;
		struct expression_delete delete;
		struct expression_control control;
		struct expression_for _for;
		struct expression_free free;
		struct expression_if _if;
		struct expression_match match;
		struct expression_measure measure;
		struct expression_return _return;
		struct expression_switch _switch;
		struct expression_struct _struct;
		struct expression_slice slice;
		struct expression_tuple tuple;
		struct expression_unarithm unarithm;
		struct expression_vaarg vaarg;
		void *user;
	};
};

#endif
