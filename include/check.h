#ifndef HARE_CHECK_H
#define HARE_CHECK_H
#include <stdbool.h>
#include "identifier.h"
#include "types.h"
#include "type_store.h"

struct expression;
struct scope;

struct context {
	struct type_store store;
	const struct type *current_fntype;
	const struct type *type_hint;
	struct identifier *ns;
	struct scope *unit;
	struct scope *scope;
	int id;
};

enum func_decl_flags {
	FN_FINI = 1 << 0,
	FN_INIT = 1 << 1,
	FN_TEST = 1 << 2,
};

struct function_decl {
	const struct type *type;
	struct expression *body;
	struct scope *scope;
	unsigned int flags; // enum function_flags
};

struct global_decl {
	const struct type *type;
	struct expression *value; // EXPR_CONSTANT
};

enum declaration_type {
	DECL_FUNC,
	DECL_TYPE,
	DECL_GLOBAL,
	DECL_CONSTANT,
};

struct declaration {
	enum declaration_type type;
	struct identifier ident;
	bool exported;
	union {
		struct function_decl func;
		struct global_decl global;
	};
};

struct declarations {
	struct declaration *decl;
	struct declarations *next;
};

struct unit {
	struct identifier *ns;
	struct declarations *declarations;
};

struct ast_expression;
struct ast_unit;

void check(struct context *ctx,
	const struct ast_unit *aunit,
	struct unit *unit);

void check_expression(struct context *ctx,
	const struct ast_expression *aexpr, struct expression *expr);

#endif
