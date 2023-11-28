#ifndef HARE_TYPESTORE_H
#define HARE_TYPESTORE_H
#include "ast.h"
#include "lex.h"
#include "types.h"

#define TYPE_STORE_BUCKETS 65536

struct type_bucket {
	struct type type;
	struct type_bucket *next;
};

struct context;

typedef struct type_bucket *type_store[TYPE_STORE_BUCKETS];

// Applies the type reduction algorithm to the given tagged union.
const struct type *type_store_reduce_result(struct context *ctx,
		struct location loc, struct type_tagged_union *in);

struct ast_type;

const struct type *type_store_lookup_atype(
	struct context *ctx, const struct ast_type *atype);

struct dimensions type_store_lookup_dimensions(
	struct context *ctx, const struct ast_type *atype);

const struct type *builtin_type_for_storage(
	enum type_storage storage, bool is_const);

const struct type *type_store_lookup_with_flags(struct context *ctx,
	const struct type *type, unsigned int flags);

const struct type *type_store_lookup_pointer(struct context *ctx,
	struct location loc, const struct type *referent, unsigned int ptrflags);

const struct type *type_store_lookup_array(struct context *ctx,
	struct location loc, const struct type *members, size_t len,
	bool expandable);

const struct type *type_store_lookup_slice(struct context *ctx,
	struct location loc, const struct type *members);

// Looks up a type alias, which may be incomplete. If the dimensions of the
// type are known, provide them as a hint in the dims argument (which can be
// NULL otherwise). This is used as a hint to skip adding padding to packed
// struct types.
const struct type *type_store_lookup_alias(struct context *ctx,
	const struct type *secondary, const struct dimensions *dims);

const struct type *type_store_lookup_tagged(struct context *ctx,
	struct location loc, struct type_tagged_union *tags);

// Returns a (non-tagged) union of the members of a tagged union type
const struct type *type_store_tagged_to_union(
	struct context *ctx, const struct type *tagged);

const struct type *type_store_lookup_tuple(struct context *ctx,
	struct location loc, struct type_tuple *values);

const struct type *type_store_lookup_enum(struct context *ctx,
	const struct ast_type *atype, bool exported);

#endif
