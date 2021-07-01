#ifndef HAREC_GEN_H
#define HAREC_GEN_H
#include <stddef.h>
#include "identifier.h"
#include "qbe.h"
#include "types.h"

enum fixed_aborts {
	ABORT_OOB = 0,
	ABORT_TYPE_ASSERTION = 1,
	ABORT_ALLOC_FAILURE = 2,
	ABORT_STATIC_EXCEEDED = 3,
};

struct unit;

void gen(const struct unit *unit, struct qbe_program *out);

struct gen_context {
	struct qbe_program *out;
	struct identifier *ns;
	struct qbe_func *current;
	uint64_t id;
};

struct gen_temp {
	char *name;
	const struct type *type;
};

// qinstr.c
enum qbe_instr alloc_for_align(size_t align);

#endif
