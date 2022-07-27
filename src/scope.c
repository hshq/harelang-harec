#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "expr.h"
#include "identifier.h"
#include "scope.h"
#include "util.h"

static uint32_t
name_hash(uint32_t init, const struct identifier *ident)
{
	return fnv1a_s(init, ident->name);
}

struct scope *
scope_push(struct scope **stack, enum scope_class class)
{
	struct scope *new = xcalloc(1, sizeof(struct scope));
	new->class = class;
	new->next = &new->objects;
	new->parent = *stack;
	*stack = new;
	return new;
}

struct scope *
scope_pop(struct scope **stack)
{
	struct scope *prev = *stack;
	assert(prev);
	*stack = prev->parent;
	return prev;
}

struct scope *
scope_lookup_ancestor(struct scope *scope,
	enum scope_class class, const char *label)
{
	// Implements the algorithm described by "Control statements" item 2, or
	// 6.6.48.2 at the time of writing
	while (scope) {
		if (label && scope->label && strcmp(scope->label, label) == 0) {
			break;
		} else if (!label && scope->class == class) {
			break;
		}
		scope = scope->parent;
	}

	if (scope && class != scope->class) {
		assert(scope->class == SCOPE_COMPOUND);
		scope = scope->parent;
	}

	return scope;
}

void
scope_free(struct scope *scope)
{
	if (!scope) {
		return;
	}

	struct scope_object *obj = scope->objects;
	while (obj) {
		struct scope_object *next = obj->lnext;
		free(obj);
		obj = next;
	}

	free(scope);
}

void
scope_free_all(struct scopes *scopes)
{
	while (scopes) {
		struct scopes *next = scopes->next;
		scope_free(scopes->scope);
		free(scopes);
		scopes = next;
	}
}

void
scope_object_init(struct scope_object *object, enum object_type otype,
	const struct identifier *ident, const struct identifier *name,
	const struct type *type, struct expression *value)
{
	identifier_dup(&object->ident, ident);
	identifier_dup(&object->name, name);
	object->otype = otype;
	object->type = type;
	object->value = value;
	if (value) {
		assert(otype == O_CONST);
		assert(value->type == EXPR_CONSTANT);
	}
	const_refer(type, &object->type);
}

void
scope_insert_from_object(struct scope *scope, struct scope_object *object)
{
	// Linked list
	*scope->next = object;
	scope->next = &object->lnext;

	// Hash map
	uint32_t hash = name_hash(FNV1A_INIT, &object->name);
	struct scope_object **bucket = &scope->buckets[hash % SCOPE_BUCKETS];
	if (*bucket) {
		object->mnext = *bucket;
	}
	*bucket = object;
}

struct scope_object *
scope_insert(struct scope *scope, enum object_type otype,
	const struct identifier *ident, const struct identifier *name,
	const struct type *type, struct expression *value)
{
	struct scope_object *o = xcalloc(1, sizeof(struct scope_object));
	scope_object_init(o, otype, ident, name, type, value);
	scope_insert_from_object(scope, o);
	return o;
}

struct scope_object *
scope_lookup(struct scope *scope, const struct identifier *ident)
{
	uint32_t hash = name_hash(FNV1A_INIT, ident);
	struct scope_object *bucket = scope->buckets[hash % SCOPE_BUCKETS];
	while (bucket) {
		if (identifier_eq(&bucket->name, ident)) {
			return bucket;
		}
		bucket = bucket->mnext;
	}
	if (scope->parent) {
		return scope_lookup(scope->parent, ident);
	}
	return NULL;
}
