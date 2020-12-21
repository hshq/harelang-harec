#ifndef HAREC_SCOPE_H
#define HAREC_SCOPE_H
#include "identifier.h"
#include "trace.h"

// XXX: This might be better as a hash map
struct scope_object {
	struct identifier ident;
	const struct type *type;
	struct scope_object *next;
};

struct scope {
	struct scope_object *objects;
	struct scope *parent;
};

struct scopes {
	struct scope *scope;
	struct scopes *next;
};

struct scope *scope_push(struct scope **stack, enum trace_sys sys);
struct scope *scope_pop(struct scope **stack, enum trace_sys sys);

void scope_free(struct scope *scope);
void scope_free_all(struct scopes *scopes);

void scope_insert(const struct identifier *ident, const struct type *type);
const struct type *scope_lookup(const struct identifier *ident);

#endif
