#ifndef HARE_AST_H
#define HARE_AST_H
#include <stdbool.h>
#include <stdint.h>
#include "check.h"
#include "expr.h"
#include "identifier.h"
#include "types.h"

enum ast_import_mode {
	AST_IMPORT_IDENTIFIER,	// use foo::bar;
	AST_IMPORT_ALIAS,	// use foo::bar = x::y;
	AST_IMPORT_MEMBERS,	// use foo::bar::{a, b, c};
};

struct ast_imports {
	enum ast_import_mode mode;
	union {
		struct identifier ident;
		struct identifier *alias;
		struct ast_imports *members;
	};
	struct ast_imports *next;
};

struct ast_list_type {
	struct ast_expression *length; // NULL for slices and unbounded arrays
	struct ast_type *members;
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

enum struct_union_member_type {
	MEMBER_TYPE_FIELD,
	MEMBER_TYPE_EMBEDDED,
	MEMBER_TYPE_ALIAS,
};

struct ast_struct_union_type {
	enum struct_union_member_type member_type;
	struct ast_struct_union_type *next;
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
	enum type_storage storage;
	unsigned int flags;
	union {
		struct identifier alias;
		struct ast_list_type array;
		struct ast_enum_type _enum;
		struct ast_function_type func;
		struct ast_pointer_type pointer;
		struct ast_list_type slice;
		struct ast_struct_union_type _struct;
		struct ast_tagged_union_type tagged_union;
		struct ast_struct_union_type _union;
	};
};

struct ast_expression_access {
	struct identifier ident;
	enum access_type type;
	// TODO: Details for index, field selection
};

struct ast_expression_assign {
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

struct ast_expression_list {
	struct ast_expression *expr;
	struct ast_expression_list *next;
};

struct ast_expression_measure {
	enum measure_operator op;
	union {
		struct ast_expression *value;
		struct ast_type *type;
		// TODO: Field selection
	};
};

struct ast_expression_return {
	struct ast_expression *value;
};

struct ast_expression_unarithm {
	enum unarithm_operator op;
	struct ast_expression *operand;
};

struct ast_expression {
	enum expr_type type;
	union {
		struct ast_expression_access access;
		struct ast_expression_assign assign;
		struct ast_expression_binarithm binarithm;
		struct ast_expression_binding binding;
		struct ast_expression_call call;
		struct ast_expression_constant constant;
		struct ast_expression_list list;
		struct ast_expression_measure measure;
		struct ast_expression_return _return;
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
