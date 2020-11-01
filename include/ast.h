#ifndef HARE_AST_H
#define HARE_AST_H
#include "identifier.h"

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

enum ast_declaration_type {
	AST_DECL_FUNC,
	AST_DECL_TYPE,
	AST_DECL_VAR,
	AST_DECL_CONST,
};

struct ast_declaration {
	enum ast_declaration_type type;
};

struct ast_declarations {
	struct ast_declaration decl;
	struct ast_declarations *next;
};

struct ast_subunit {
	struct ast_imports *imports;
	struct ast_declarations decls;
	struct ast_subunit *next;
};

struct ast_unit {
	struct identifier *ns;
	struct ast_subunit subunits;
};

#endif
