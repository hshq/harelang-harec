#ifndef HARE_AST_H
#define HARE_AST_H
#include <stdbool.h>
#include <stdint.h>
#include "expr.h"
#include "identifier.h"
#include "lex.h"
#include "types.h"

struct ast_type;

enum ast_import_mode {
	AST_IMPORT_IDENTIFIER,	// use foo::bar;
	AST_IMPORT_ALIAS,	// use foo::bar = x::y;
	AST_IMPORT_MEMBERS,	// use foo::bar::{a, b, c};
};

struct ast_imports {
	struct location loc;
	enum ast_import_mode mode;
	struct identifier ident;
	struct identifier *alias;
	struct ast_imports *members;
	struct ast_imports *next;
};

struct ast_list_type {
	struct ast_expression *length; // NULL for slices and unbounded arrays
	struct ast_type *members;
	bool contextual;
};

struct ast_enum_field {
	const char *name;
	struct ast_expression *value;
	struct ast_enum_field *next;
};

struct ast_enum_type {
	enum type_storage storage;
	struct ast_enum_field *values;
};

struct ast_function_parameters {
	struct location loc;
	char *name;
	struct ast_type *type;
	struct ast_function_parameters *next;
};

struct ast_function_type {
	struct ast_type *result;
	struct ast_function_parameters *params;
	enum variadism variadism;
	unsigned int flags; // enum function_flags (types.h)
};

struct ast_pointer_type {
	struct ast_type *referent;
	unsigned int flags;
};

struct ast_tagged_union_type {
	struct ast_type *type;
	struct ast_tagged_union_type *next;
};

struct ast_tuple_type {
	struct ast_type *type;
	struct ast_tuple_type *next;
};

enum struct_union_member_type {
	MEMBER_TYPE_FIELD,
	MEMBER_TYPE_EMBEDDED,
	MEMBER_TYPE_ALIAS,
};

struct ast_struct_union_type {
	enum struct_union_member_type member_type;
	struct ast_struct_union_type *next;
	struct ast_expression *offset;
	union {
		struct {
			char *name;
			struct ast_type *type;
		} field;
		struct ast_type *embedded;
		struct identifier alias;
	};
};

struct ast_type {
	struct location loc;
	enum type_storage storage;
	unsigned int flags;
	union {
		struct ast_list_type array;
		struct ast_enum_type _enum;
		struct ast_function_type func;
		struct ast_pointer_type pointer;
		struct ast_list_type slice;
		struct ast_struct_union_type struct_union;
		struct ast_tagged_union_type tagged_union;
		struct ast_tuple_type tuple;
		struct {
			struct identifier alias;
			bool unwrap;
		};
	};
};

struct ast_expression_access {
	enum access_type type;
	union {
		struct identifier ident;
		struct {
			struct ast_expression *array;
			struct ast_expression *index;
		};
		struct {
			struct ast_expression *_struct;
			char *field;
		};
		struct {
			struct ast_expression *tuple;
			struct ast_expression *value;
		};
	};
};

struct ast_expression_alloc {
	struct ast_expression *expr;
	struct ast_expression *cap;
};

struct ast_append_values {
	struct ast_expression *expr;
	struct ast_append_values *next;
};

struct ast_expression_append {
	struct ast_expression *expr;
	struct ast_expression *variadic;
	struct ast_append_values *values;
};

struct ast_expression_assert {
	struct ast_expression *cond;
	struct ast_expression *message;
	bool is_static;
};

struct ast_expression_assign {
	enum binarithm_operator op;
	struct ast_expression *object, *value;
	bool indirect;
};

struct ast_expression_binarithm {
	enum binarithm_operator op;
	struct ast_expression *lvalue, *rvalue;
};

struct ast_expression_binding {
	char *name;
	struct ast_type *type;
	unsigned int flags;
	bool is_static;
	struct ast_expression *initializer;
	struct ast_expression_binding *next;
};

struct ast_call_argument {
	bool variadic;
	struct ast_expression *value;
	struct ast_call_argument *next;
};

struct ast_expression_call {
	struct ast_expression *lvalue;
	struct ast_call_argument *args;
};

struct ast_expression_cast {
	enum cast_kind kind;
	struct ast_expression *value;
	struct ast_type *type;
};

struct ast_array_constant {
	struct ast_expression *value;
	struct ast_array_constant *next;
	bool expand;
};

struct ast_expression_constant {
	enum type_storage storage;
	union {
		intmax_t ival;
		uintmax_t uval;
		uint32_t rune;
		bool bval;
		struct {
			size_t len;
			char *value;
		} string;
		struct ast_array_constant *array;
	};
};

struct ast_expression_control {
	char *label;
};

struct ast_expression_defer {
	struct ast_expression *deferred;
};

struct ast_expression_delete {
	struct ast_expression *expr;
};

struct ast_expression_for {
	char *label;
	struct location label_loc;
	struct ast_expression *bindings;
	struct ast_expression *cond;
	struct ast_expression *afterthought;
	struct ast_expression *body;
};

struct ast_expression_free {
	struct ast_expression *expr;
};

struct ast_expression_if {
	struct ast_expression *cond;
	struct ast_expression *true_branch, *false_branch;
};

struct ast_expression_list {
	struct ast_expression *expr;
	struct ast_expression_list *next;
};

struct ast_match_case {
	char *name; // May be null
	struct ast_type *type;
	struct ast_expression *value;
	struct ast_match_case *next;
};

struct ast_expression_match {
	struct ast_expression *value;
	struct ast_match_case *cases;
};

struct ast_expression_measure {
	enum measure_operator op;
	union {
		struct ast_expression *value;
		struct ast_type *type;
		// TODO: Field selection
	};
};

struct ast_expression_propagate {
	struct ast_expression *value;
};

struct ast_expression_return {
	struct ast_expression *value;
};

struct ast_expression_slice {
	struct ast_expression *object;
	struct ast_expression *start, *end;
};

struct ast_case_option {
	struct ast_expression *value;
	struct ast_case_option *next;
};

struct ast_switch_case {
	struct ast_case_option *options; // NULL for *
	struct ast_expression *value;
	struct ast_switch_case *next;
};

struct ast_expression_switch {
	struct ast_expression *value;
	struct ast_switch_case *cases;
};

struct ast_expression_struct;

struct ast_field_value {
	bool is_embedded;
	union {
		struct {
			char *name;
			struct ast_type *type;
			struct ast_expression *initializer;
		} field;
		struct ast_expression *embedded;
	};
	struct ast_field_value *next;
};

struct ast_expression_struct {
	bool autofill;
	struct identifier type;
	struct ast_field_value *fields;
};

struct ast_expression_tuple {
	struct ast_expression *expr;
	struct ast_expression_tuple *next;
};

struct ast_expression_unarithm {
	enum unarithm_operator op;
	struct ast_expression *operand;
};

struct ast_expression {
	struct location loc;
	enum expr_type type;
	union {
		struct ast_expression_access access;
		struct ast_expression_alloc alloc;
		struct ast_expression_append append;
		struct ast_expression_assert assert;
		struct ast_expression_assign assign;
		struct ast_expression_binarithm binarithm;
		struct ast_expression_binding binding;
		struct ast_expression_call call;
		struct ast_expression_cast cast;
		struct ast_expression_constant constant;
		struct ast_expression_control control;
		struct ast_expression_defer defer;
		struct ast_expression_delete delete;
		struct ast_expression_for _for;
		struct ast_expression_free free;
		struct ast_expression_if _if;
		struct ast_expression_list list;
		struct ast_expression_match match;
		struct ast_expression_measure measure;
		struct ast_expression_propagate propagate;
		struct ast_expression_return _return;
		struct ast_expression_slice slice;
		struct ast_expression_struct _struct;
		struct ast_expression_switch _switch;
		struct ast_expression_tuple tuple;
		struct ast_expression_unarithm unarithm;
	};
};

struct ast_global_decl {
	char *symbol;
	struct identifier ident;
	struct ast_type *type;
	struct ast_expression *init;
	struct ast_global_decl *next;
};

struct ast_type_decl {
	struct identifier ident;
	struct ast_type *type;
	struct ast_type_decl *next;
};

struct ast_function_decl {
	char *symbol;
	struct identifier ident;
	struct ast_function_type prototype;
	struct ast_expression *body;
	unsigned int flags; // enum func_decl_flags (check.h)
};

enum ast_decl_type {
	AST_DECL_FUNC,
	AST_DECL_TYPE,
	AST_DECL_GLOBAL,
	AST_DECL_CONST,
};

struct ast_decl {
	struct location loc;
	enum ast_decl_type decl_type;
	bool exported;
	union {
		struct ast_global_decl global;
		struct ast_global_decl constant;
		struct ast_type_decl type;
		struct ast_function_decl function;
	};
};

struct ast_decls {
	struct ast_decl decl;
	struct ast_decls *next;
};

struct ast_subunit {
	struct ast_imports *imports;
	struct ast_decls decls;
	struct ast_subunit *next;
};

struct ast_unit {
	struct identifier *ns;
	struct ast_subunit subunits;
};

#endif
