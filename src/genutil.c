#include <stdio.h>
#include <stdlib.h>
#include "gen.h"
#include "qbe.h"
#include "util.h"

char *
gen_name(struct gen_context *ctx, const char *fmt)
{
	int n = snprintf(NULL, 0, fmt, ctx->id);
	char *str = xcalloc(1, n + 1);
	snprintf(str, n + 1, fmt, ctx->id);
	++ctx->id;
	return str;
}

// Initializes a qval with a qbe temporary for a given gen temporary.
void
qval_temp(struct gen_context *ctx,
	struct qbe_value *out,
	const struct gen_temp *temp)
{
	if (temp->is_global) {
		out->kind = QV_GLOBAL;
	} else {
		out->kind = QV_TEMPORARY;
	}
	out->type = qtype_lookup(ctx, temp->type, true);
	out->name = temp->name;
}

// Initializes a qbe_value as a qbe temporary for the given qbe type.
void
gen_qtemp(struct gen_context *ctx, struct qbe_value *out,
		const struct qbe_type *type, const char *fmt)
{
	out->kind = QV_TEMPORARY;
	out->type = type;
	out->name = gen_name(ctx, fmt);
}

// Generates a direct temporary of the given type, which must be a primitive
// type.
void
gen_direct(struct gen_context *ctx, struct gen_temp *temp,
		const struct type *type, const char *fmt)
{
	// Functions are a special case: the Hare type system (correctly) states
	// that they have an undefined size and storage, but we can assign them
	// to qbe temporaries as pointers (=l), so they are suitable for use as
	// direct objects.
	if (type_dealias(type)->storage != STORAGE_FUNCTION) {
		assert(type->size != 0 && type->size != SIZE_UNDEFINED);
		const struct qbe_type *qtype = qtype_lookup(ctx, type, false);
		assert(qtype->stype != Q__AGGREGATE && qtype->stype != Q__VOID);
	}

	temp->type = type;
	temp->name = gen_name(ctx, fmt);
	temp->indirect = false;
}

// Emits a qbe copy instruction which makes a working copy of a gen temporary.
void
temp_workcopy(struct gen_context *ctx, struct qbe_value *qval,
		const struct qbe_type *qtype,
		const struct gen_temp *temp, const char *fmt)
{
	struct qbe_value qtemp = {0};
	gen_qtemp(ctx, qval, qtype, fmt);
	qval_temp(ctx, &qtemp, temp);
	pushi(ctx->current, qval, Q_COPY, &qtemp, NULL);
}

// Allocates a temporary of the given type on the stack in the current
// function's preamble.
void
alloc_temp(struct gen_context *ctx, struct gen_temp *temp,
		const struct type *type, const char *fmt)
{
	assert(type->size != 0 && type->size != SIZE_UNDEFINED);
	temp->type = type;
	temp->name = gen_name(ctx, fmt);
	temp->indirect = true;

	struct qbe_value out = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = temp->name,
	};
	struct qbe_value size;
	constl(&size, type->size);
	pushprei(ctx->current, &out, alloc_for_align(type->align), &size, NULL);
}

// Loads a gen temporary into a qbe temporary. For types representable in qbe's
// type system, this loads the actual value into a qbe temporary. Otherwise,
// this behaves equivalently to qval_temp, but sets the temporary type to the
// platform's pointer type (e.g. =l).
void
load_temp(struct gen_context *ctx,
	struct qbe_value *out,
	const struct gen_temp *temp)
{
	// TODO: Audit me
	const struct qbe_type *qtype = qtype_lookup(ctx, temp->type, true);
	assert(qtype->stype != Q__VOID);

	out->kind = QV_TEMPORARY;
	if (qtype->stype == Q__AGGREGATE) {
		assert(temp->indirect);
		out->name = temp->name;
		out->type = ctx->arch.ptr;
	} else {
		out->name = gen_name(ctx, "load.%d");
		out->type = qtype;

		struct qbe_value src;
		qval_temp(ctx, &src, temp);
		if (temp->indirect) {
			enum qbe_instr instr = load_for_type(ctx, temp->type);
			pushi(ctx->current, out, instr, &src, NULL);
		} else {
			pushi(ctx->current, out, Q_COPY, &src, NULL);
		}
	}
}

void
store_temp(struct gen_context *ctx,
	const struct gen_temp *temp,
	struct qbe_value *value)
{
	struct qbe_value out;
	qval_temp(ctx, &out, temp);

	if (temp->indirect) {
		enum qbe_instr instr = store_for_type(ctx, temp->type);
		pushi(ctx->current, NULL, instr, value, &out, NULL);
	} else {
		pushi(ctx->current, &out, Q_COPY, value, NULL);
	}
}

// Obtains the address of a temporary and changes it to the given pointer type.
void
temp_address(struct gen_temp *temp, const struct type *type)
{
	assert(type_dealias(type)->storage == STORAGE_POINTER);
	assert(temp->indirect || temp->is_global);
	temp->indirect = false;
	temp->type = type;
}

// Dereferences a temporary, changing it to an indirect address to its referent
// type.
void
temp_deref(struct gen_temp *temp)
{
	assert(type_dealias(temp->type)->storage == STORAGE_POINTER);
	assert(!temp->indirect);
	temp->indirect = true;
	temp->type = temp->type->pointer.referent;
}

const struct gen_binding *
binding_lookup(struct gen_context *ctx, const struct scope_object *obj)
{
	for (struct gen_binding *binding = ctx->bindings;
			binding; binding = binding->next) {
		if (binding->object == obj) {
			return binding;
		}
	}
	abort(); // Invariant
}
