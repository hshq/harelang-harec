#ifndef HAREC_SCOPE_H
#define HAREC_SCOPE_H
#include "expr.h"
#include "identifier.h"

#define SCOPE_BUCKETS 4096

enum object_type {
	O_BIND,
	O_CONST,
	O_DECL,
	O_SCAN,
	O_TYPE,
};

struct scope_object {
	enum object_type otype;
	// name is the name of the object within this scope (for lookups)
	// ident is the global identifier (these may be different in some cases)
	struct identifier name, ident;
	bool threadlocal;

	union {
		const struct type *type;
		struct expression *value; // For O_CONST
	};

	struct scope_object *lnext; // Linked list
	struct scope_object *mnext; // Hash map
};

enum scope_class {
	SCOPE_COMPOUND,
	SCOPE_DEFER,
	SCOPE_ENUM,
	SCOPE_FUNC,
	SCOPE_LOOP,
	SCOPE_MATCH,
	SCOPE_SUBUNIT,
	SCOPE_UNIT,
	SCOPE_DEFINES,
};

struct yield {
	struct expression **expression;
	struct yield *next;
};

struct scope {
	// Used for for loops
	bool has_break;

	enum scope_class class;
	const char *label;
	struct scope *parent;

	const struct type *hint;
	struct type_tagged_union *results;
	struct yield *yields;

	// Linked list in insertion order
	// Used for function parameters and enum values, where order matters
	struct scope_object *objects;
	struct scope_object **next;

	// Hash map in reverse insertion order
	// Used for lookups, and accounts for shadowing
	struct scope_object *buckets[SCOPE_BUCKETS];
};

struct scopes {
	struct scope *scope;
	struct scopes *next;
};

struct scope *scope_push(struct scope **stack, enum scope_class class);
struct scope *scope_pop(struct scope **stack);

struct scope *scope_lookup_class(struct scope *scope, enum scope_class class);
struct scope *scope_lookup_label(struct scope *scope, const char *label);

void scope_free(struct scope *scope);
void scope_free_all(struct scopes *scopes);

void scope_object_init(struct scope_object *obj, enum object_type otype,
	const struct identifier *ident, const struct identifier *name,
	const struct type *type, struct expression *value);

void scope_insert_from_object(struct scope *scope, struct scope_object *object);

struct scope_object *scope_insert(
	struct scope *scope, enum object_type otype,
	const struct identifier *ident, const struct identifier *name,
	const struct type *type, struct expression *value);

struct scope_object *scope_lookup(struct scope *scope,
	const struct identifier *ident);

#endif
