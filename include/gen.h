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

struct gen_temp {
	char *name;
	const struct type *type;
};

struct gen_arch {
	const struct qbe_type *ptr;
	const struct qbe_type *sz;
};

struct gen_context {
	struct qbe_program *out;
	struct type_store *store;
	struct identifier *ns;
	struct qbe_func *current;
	const struct type *functype;
	const char *end;
	struct gen_temp *rval;
	struct gen_arch arch;
	uint64_t id;
};

struct unit;
void gen(const struct unit *unit,
		struct type_store *store,
		struct qbe_program *out);

// qinstr.c
enum qbe_instr alloc_for_align(size_t align);
enum qbe_instr store_for_type(struct gen_context *ctx, const struct type *type);
enum qbe_instr load_for_type(struct gen_context *ctx, const struct type *type);

// qtype.c
const struct qbe_type *qtype_lookup(struct gen_context *ctx,
		const struct type *type, bool xtype);

#endif
