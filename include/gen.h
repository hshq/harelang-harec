#ifndef HAREC_GEN_H
#define HAREC_GEN_H
#include <stddef.h>
#include "identifier.h"
#include "qbe.h"
#include "type_store.h"
#include "types.h"

enum fixed_aborts {
	ABORT_OOB = 0,
	ABORT_TYPE_ASSERTION = 1,
	ABORT_ALLOC_FAILURE = 2,
	ABORT_STATIC_EXCEEDED = 3,
	ABORT_UNREACHABLE = 4,
};

struct gen_arch {
	const struct qbe_type *ptr;
	const struct qbe_type *sz;
};

enum gen_value_kind {
	GV_CONST,
	GV_GLOBAL,
	GV_TEMP,
};

struct gen_value {
	enum gen_value_kind kind;
	const struct type *type;
	union {
		char *name;
		uint32_t wval;
		uint64_t lval;
		float sval;
		double dval;
	};
};

struct gen_binding {
	const struct scope_object *object;
	struct gen_value value;
	struct gen_binding *next;
};

struct gen_defer {
	const struct expression *expr;
	struct gen_defer *next;
};

struct gen_scope {
	const char *label;
	struct qbe_value *after;
	struct qbe_value *end;
	struct gen_defer *defers;
	struct gen_scope *parent;
};

struct gen_context {
	struct qbe_program *out;
	struct gen_arch arch;
	struct type_store *store;
	struct identifier *ns;

	uint64_t id;

	struct qbe_func *current;
	const struct type *functype;
	struct gen_binding *bindings;
	struct gen_scope *scope;
};

struct unit;

void gen(const struct unit *unit,
		struct type_store *store,
		struct qbe_program *out);

// genutil.c
char *gen_name(struct gen_context *ctx, const char *fmt);
struct gen_value mktemp(struct gen_context *ctx,
	const struct type *type, const char *fmt);
struct qbe_value mkqval(struct gen_context *ctx, struct gen_value *value);
struct qbe_value mklval(struct gen_context *ctx, struct gen_value *value);
struct qbe_value mkcopy(struct gen_context *ctx,
	struct gen_value *value, const char *fmt);
struct qbe_value mkqtmp(struct gen_context *ctx,
	const struct qbe_type *qtype, const char *fmt);
struct qbe_value mkrtfunc(struct gen_context *ctx, const char *name);
struct qbe_value mklabel(struct gen_context *ctx,
	struct qbe_statement *stmt, const char *fmt);
void branch_copyresult(struct gen_context *ctx, struct gen_value result,
	struct gen_value merged, struct gen_value *out);

// qinstr.c
enum qbe_instr alloc_for_align(size_t align);
enum qbe_instr store_for_type(struct gen_context *ctx, const struct type *type);
enum qbe_instr load_for_type(struct gen_context *ctx, const struct type *type);
enum qbe_instr binarithm_for_op(struct gen_context *ctx,
	enum binarithm_operator op, const struct type *type);

// qtype.c
const struct qbe_type *qtype_lookup(struct gen_context *ctx,
	const struct type *type, bool xtype);
bool type_is_aggregate(const struct type *type);

#endif
