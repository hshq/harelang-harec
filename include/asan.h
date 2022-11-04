#ifndef HAREC_ASAN_H
#define HAREC_ASAN_H
#include "gen.h"
#include "qbe.h"
#include "types.h"

void gen_asan_redzone(
	struct gen_context *ctx,
	struct qbe_value *out,
	const struct type *type);

void gen_asan_loadcheck(
	struct gen_context *ctx,
	enum qbe_instr instr,
	struct qbe_value *addr);

void gen_asan_redzone_cleanup(
	struct gen_context *ctx,
	struct gen_redzone *redzone);

#endif
