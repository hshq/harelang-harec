#ifndef HAREC_GEN_H
#define HAREC_GEN_H
#include <stdio.h>
#include <stdint.h>
#include "identifier.h"
#include "qbe.h"

struct unit;

void gen(const struct unit *unit, struct qbe_program *out);

struct gen_context {
	struct qbe_program *out;
	struct identifier *ns;
	struct qbe_func *current;
	uint64_t id;
};

struct type;

// qtype.c
enum qbe_stype qstype_for_type(const struct type *type);
enum qbe_stype qxtype_for_type(const struct type *type);
const struct qbe_type *qtype_for_type(struct gen_context *ctx,
		const struct type *type, bool extended);

// qinstr.c
enum qbe_instr alignment_to_qbe_alloc(size_t align);

#endif
