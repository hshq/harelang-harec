#include <assert.h>
#include "gen.h"
#include "qbe.h"
#include "types.h"

const struct qbe_type *qtype_lookup(
		struct gen_context *ctx,
		const struct type *type) {
	// TODO
	assert(type->storage == STORAGE_INT);
	return &qbe_word;
}
