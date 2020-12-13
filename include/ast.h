#ifndef HARE_AST_H
#define HARE_AST_H
#include <stdbool.h>
#include <stdint.h>
#include "check.h"
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
	struct type *members;
};

struct ast_enum_field {
	const char *name;
	struct ast_expression *value;
	struct enum_field *next;
};

struct ast_enum_type {
	enum type_storage storage;
	struct ast_enum_field *values;
};

enum variadism {
	VARIADISM_NONE,
	VARIADISM_C,
	VARIADISM_HARE,
};

struct ast_function_parameters {
	char *name;
	struct ast_type *type;
	struct ast_function_parameters *next;
};

struct ast_function_type {
	bool noreturn;
	enum variadism variadism;
	struct ast_type *result;
	struct ast_function_parameters *parameters;
};

struct ast_pointer_type {
	struct ast_type *referent;
	unsigned int flags;
};

struct ast_tagged_union_type {
	struct ast_type *type;
	struct ast_tagged_union_type *next;
};

struct ast_struct_union_type {
	const char *name;
	struct ast_type *type;
	struct ast_struct_union_type *next;
};

struct ast_type {
	enum type_storage storage;
	unsigned int flags;
	union {
		struct identifier alias;
		struct ast_list_type array;
		struct ast_enum_type _enum;
		struct ast_function_type function;
		struct ast_pointer_type pointer;
		struct ast_list_type slice;
		struct ast_struct_union_type _struct;
		struct ast_tagged_union_type tagged_union;
		struct ast_struct_union_type _union;
	};
};

enum expression_type {
	EXPR_CONSTANT,
};

struct ast_constant_expression {
	enum type_storage storage;
	union {
		intmax_t _signed;
		uintmax_t _unsigned;
		struct {
			size_t len;
			char *value;
		} string;
	};
};

struct ast_expression {
	enum expression_type type;
	union {
		struct ast_constant_expression constant;
	};
};

struct ast_global_decl {
	char *symbol;
	struct identifier ident;
	struct ast_type type;
	struct ast_expression init;
	struct ast_global_decl *next;
};

struct ast_type_decl {
	struct identifier ident;
	struct ast_type type;
	struct ast_type_decl *next;
};

struct ast_function_decl {
	char *symbol;
	uint32_t flags; // enum function_flags (check.h)
	struct identifier ident;
	struct ast_function_type prototype;
	struct ast_expression body;
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
