#ifndef HARE_CHECK_H
#define HARE_CHECK_H
#include <stdbool.h>
#include "identifier.h"
#include "scope.h"
#include "types.h"
#include "type_store.h"

struct expression;

#define MODCACHE_BUCKETS 256

struct modcache {
	struct identifier ident;
	struct scope *scope;
	struct modcache *next;
};

struct ast_expression;
struct ast_unit;

struct errors {
	struct location loc;
	char *msg;
	struct errors *next;
};

struct context {
	struct modcache **modcache;
	struct type_store *store;
	const struct type *fntype;
	struct identifier *ns;
	bool is_test;
	struct scope *unit;
	struct scope *scope;
	struct scope *defines;
	bool deferring;
	int id;
	struct errors *errors;
	struct errors **next;
	struct declarations *decls;
};

struct constant_decl {
	const struct type *type;
	const struct expression *value;
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
	bool threadlocal;
};

struct declaration {
	enum decl_type decl_type;
	struct identifier ident;
	struct location loc;
	char *symbol;
	bool exported;
	union {
		struct constant_decl constant;
		struct function_decl func;
		struct global_decl global;
		const struct type *type;
	};
};

struct declarations {
	struct declaration decl;
	struct declarations *next;
};

struct unit {
	struct identifier *ns;
	struct declarations *declarations;
	struct identifiers *imports;
};

enum idecl_type {
	IDECL_DECL,
	IDECL_ENUM_FLD,
};

// Keeps track of enum specific context required for enum field resolution
struct incomplete_enum_field {
	struct ast_enum_field *field;
	struct scope *enum_scope;
};

// Keeps track of context required to resolve a declaration or an enum field
// Extends the scope_object struct so it can be inserted into a scope
struct incomplete_declaration {
	struct scope_object obj;
	struct scope *imports; // the scope of this declaration's subunit
	enum idecl_type type;
	bool in_progress;
	union {
		struct ast_decl decl;
		struct incomplete_enum_field *field;
	};
};

void mkident(struct context *ctx, struct identifier *out,
		const struct identifier *in, const char *symbol);

typedef void (*resolvefn)(struct context *,
		struct incomplete_declaration *idecl);

void resolve_dimensions(struct context *ctx,
		struct incomplete_declaration *idecl);

void resolve_type(struct context *ctx,
		struct incomplete_declaration *idecl);

void resolve_decl(struct context *ctx,
		struct incomplete_declaration *idecl);

void wrap_resolver(struct context *ctx,
	const struct scope_object *obj, resolvefn resolver);

struct scope *check(struct type_store *ts,
	bool is_test,
	struct ast_global_decl *defines,
	const struct ast_unit *aunit,
	struct unit *unit);

struct scope *check_internal(struct type_store *ts,
	struct modcache **cache,
	bool is_test,
	struct ast_global_decl *defines,
	const struct ast_unit *aunit,
	struct unit *unit,
	bool scan_only);

void check_expression(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint);

#endif
