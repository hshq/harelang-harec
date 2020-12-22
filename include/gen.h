#ifndef HAREC_GEN_H
#define HAREC_GEN_H
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include "identifier.h"
#include "qbe.h"

struct unit;

void gen(const struct unit *unit, struct qbe_program *out);

struct gen_binding {
	const struct scope_object *object;
	char *name;
	struct gen_binding *next;
};

struct gen_context {
	struct qbe_program *out;
	struct identifier *ns;
	struct qbe_func *current;
	const struct qbe_value *end_label;
	const struct qbe_value *return_value;
	struct gen_binding *bindings;
	uint64_t id;
};

struct type;

// qtype.c
enum qbe_stype qstype_for_type(const struct type *type);
enum qbe_stype qxtype_for_type(const struct type *type);
const struct qbe_type *qtype_for_type(struct gen_context *ctx,
		const struct type *type, bool extended);
bool type_is_aggregate(const struct type *type);

// qinstr.c
enum qbe_instr alloc_for_align(size_t align);
enum qbe_instr store_for_type(enum qbe_stype stype);
enum qbe_instr load_for_type(enum qbe_stype stype, bool is_signed);

#endif
