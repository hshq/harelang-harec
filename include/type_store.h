#ifndef HARE_TYPESTORE_H
#define HARE_TYPESTORE_H
#include "ast.h"
#include "types.h"

#define TYPE_STORE_BUCKETS 256

struct types {
	struct type type;
	struct types *next;
};

struct type_store {
	struct type buckets[TYPE_STORE_BUCKETS];
};

unsigned long atype_hash(struct type_store *store, const struct ast_type *type);
unsigned long type_hash(struct type_store *store, const struct type *type);

#endif
