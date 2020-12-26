#ifndef HARE_TYPESTORE_H
#define HARE_TYPESTORE_H
#include "ast.h"
#include "types.h"

#define TYPE_STORE_BUCKETS 256

struct type_bucket {
	struct type type;
	struct type_bucket *next;
};

struct context;

struct type_store {
	struct type_bucket *buckets[TYPE_STORE_BUCKETS];
	struct context *check_context;
};

bool type_is_assignable(struct type_store *store,
	const struct type *to, const struct type *from);

unsigned long atype_hash(struct type_store *store, const struct ast_type *type);
unsigned long type_hash(struct type_store *store, const struct type *type);

const struct type *type_store_lookup_atype(
	struct type_store *store, const struct ast_type *atype);

const struct type *builtin_type_for_storage(
	enum type_storage storage, bool is_const);

const struct type *type_store_lookup_with_flags(struct type_store *store,
	const struct type *type, unsigned int flags);

const struct type *type_store_lookup_pointer(struct type_store *store,
	const struct type *referent, unsigned int ptrflags);

#endif
