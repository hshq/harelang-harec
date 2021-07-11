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
};

// A gen temporary is a reference to a qbe temporary by name, and the
// corresponding Hare type. If indirect is true, the qbe temporary is a pointer
// to the actual storage; otherwise the type will be representable as a qbe
// primitive.
struct gen_temp {
	char *name;
	const struct type *type;
	bool indirect;
};

// A gen binding stores the gen_temp for a scope object and is part of a linked
// list of objects being tracked by gen.
struct gen_binding {
	const struct scope_object *object;
	struct gen_temp temp;
	struct gen_binding *next;
};

struct gen_arch {
	const struct qbe_type *ptr;
	const struct qbe_type *sz;
};

struct gen_context {
	struct qbe_program *out;
	struct gen_arch arch;
	struct type_store *store;
	struct identifier *ns;

	uint64_t id;
	struct gen_binding *bindings;

	struct qbe_func *current;
	const struct type *functype;
	const char *end;
	struct gen_temp *rval;
};

struct unit;

void gen(const struct unit *unit,
		struct type_store *store,
		struct qbe_program *out);

// genutil.c
char *gen_name(struct gen_context *ctx, const char *fmt);
void qval_temp(struct gen_context *ctx, struct qbe_value *out,
	const struct gen_temp *temp);
void gen_qtemp(struct gen_context *ctx, struct qbe_value *out,
	const struct qbe_type *type, const char *fmt);
void gen_direct(struct gen_context *ctx, struct gen_temp *temp,
	const struct type *type, const char *fmt);
void temp_workcopy(struct gen_context *ctx, struct qbe_value *qval,
	const struct qbe_type *qtype, const struct gen_temp *temp,
	const char *fmt);
void alloc_temp(struct gen_context *ctx, struct gen_temp *temp,
	const struct type *type, const char *fmt);
void load_temp(struct gen_context *ctx, struct qbe_value *out,
	const struct gen_temp *temp);
void temp_address(struct gen_temp *temp, const struct type *type);
void temp_deref(struct gen_temp *temp);
const struct gen_binding *binding_lookup(struct gen_context *ctx,
	const struct scope_object *obj);

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
