#include <assert.h>
#include <stdlib.h>
#include "expr.h"
#include "identifier.h"
#include "scope.h"
#include "trace.h"
#include "util.h"

struct scope *
scope_push(struct scope **stack, enum trace_sys sys)
{
	struct scope *new = xcalloc(1, sizeof(struct scope));
	new->next = &new->objects;
	if (*stack) {
		new->parent = *stack;
	}
	*stack = new;
	if (sys != TR_MAX) {
		trenter(sys, "scope %p", new);
	}
	return new;
}

struct scope *
scope_pop(struct scope **stack, enum trace_sys sys)
{
	struct scope *prev = *stack;
	assert(prev);
	*stack = prev->parent;
	if (sys != TR_MAX) {
		trleave(sys, NULL);
	}
	return prev;
}

void
scope_free(struct scope *scope)
{
	if (!scope) {
		return;
	}

	struct scope_object *obj = scope->objects;
	while (obj) {
		struct scope_object *next = obj->next;
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

const struct scope_object *
scope_insert(struct scope *scope, enum object_type otype,
	const struct identifier *ident, const struct identifier *name,
	const struct type *type, struct expression *value)
{
	struct scope_object *o = xcalloc(1, sizeof(struct scope_object));
	identifier_dup(&o->ident, ident);
	identifier_dup(&o->name, name);
	o->otype = otype;
	o->type = type;
	o->value = value;
	if (value) {
		assert(otype == O_CONST);
		assert(value->type == EXPR_CONSTANT);
	}
	*scope->next = o;
	scope->next = &o->next;
	o->prev = scope->last;
	scope->last = o;
	return o;
}

const struct scope_object *
scope_lookup(struct scope *scope, const struct identifier *ident)
{
	struct scope_object *o = scope->last;
	while (o) {
		if (identifier_eq(&o->ident, ident)
				|| identifier_eq(&o->name, ident)) {
			return o;
		}
		o = o->prev;
	}
	if (scope->parent) {
		return scope_lookup(scope->parent, ident);
	}
	return NULL;
}
