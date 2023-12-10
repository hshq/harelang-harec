#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include "gen.h"
#include "qbe.h"
#include "type_store.h"
#include "types.h"
#include "util.h"

static const struct qbe_type *
tagged_qtype(struct gen_context *ctx, const struct type *type)
{
	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->kind = Q_TYPE;
	def->name = gen_name(&ctx->id, "tags.%d");
	def->exported = false;
	def->type.stype = Q__AGGREGATE;
	def->type.base = NULL;
	def->type.name = def->name;
	def->type.size = type->size - type->align;

	struct qbe_field *field = &def->type.fields;
	struct qbe_field **next = &field->next;
	for (const struct type_tagged_union *tu = &type->tagged;
			tu; tu = tu->next) {
		if (tu->type->size == 0) {
			if (!tu->next && *next) {
				free(*next);
				*next = NULL;
			}
			continue;
		}
		field->type = qtype_lookup(ctx, tu->type, true);
		field->count = 1;
		if (tu->next) {
			field->next = xcalloc(1, sizeof(struct qbe_field));
			next = &field->next;
			field = field->next;
		}
	}

	qbe_append_def(ctx->out, def);
	return &def->type;
}

static const struct qbe_type *
aggregate_lookup(struct gen_context *ctx, const struct type *type)
{
	for (struct qbe_def *def = ctx->out->defs; def; def = def->next) {
		if (def->kind == Q_TYPE && def->type.base == type) {
			return &def->type;
		}
	}

	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->kind = Q_TYPE;
	def->name = gen_name(&ctx->id, "type.%d");
	def->type.stype = Q__AGGREGATE;
	def->type.base = type;
	def->type.name = def->name;

	const struct type *final = type_dealias(NULL, type);
	assert((final->storage == STORAGE_STRUCT && final->struct_union.packed)
			|| type->size == SIZE_UNDEFINED
			|| type->size == 0
			|| type->size % type->align == 0);

	struct qbe_field *field = &def->type.fields;
	switch (type->storage) {
	case STORAGE_ARRAY:
		if (type->array.length == SIZE_UNDEFINED) {
			free(def->name);
			free(def);
			return &qbe_long; // Special case
		}
		field->count = type->array.length;
		field->type = qtype_lookup(ctx, type->array.members, true);
		break;
	case STORAGE_STRING:
		// XXX: This assertion does not hold for all architectures
		assert(ctx->arch.ptr->stype == ctx->arch.sz->stype);
		field->type = ctx->arch.ptr;
		field->count = 3;
		break;
	case STORAGE_SLICE:
		// XXX: This assertion does not hold for all architectures
		assert(ctx->arch.ptr->stype == ctx->arch.sz->stype);
		field->type = ctx->arch.ptr;
		field->count = 3;
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		if (!type->struct_union.c_compat) {
			field->type = NULL;
			field->count = type->size;
			break;
		}
		for (struct struct_field *tfield = type->struct_union.fields;
				tfield; tfield = tfield->next) {
			if (tfield->type->size != 0) {
				field->type =
					qtype_lookup(ctx, tfield->type, true);
				field->count = 1;
			}

			if (tfield->next && tfield->next->type->size != 0) {
				field->next = xcalloc(1, sizeof(struct qbe_field));
				field = field->next;
			}
		}
		break;
	case STORAGE_TUPLE:
		for (const struct type_tuple *tuple = &type->tuple;
				tuple; tuple = tuple->next) {
			if (tuple->type->size == 0) {
				continue;
			}
			field->type = qtype_lookup(ctx, tuple->type, true);
			field->count = 1;

			if (tuple->next) {
				field->next = xcalloc(1, sizeof(struct qbe_field));
				field = field->next;
			}
		}
		break;
	case STORAGE_TAGGED:
		field->type = &qbe_word;
		field->count = 1;
		if (type->size != builtin_type_u32.size) {
			field->next = xcalloc(1, sizeof(struct qbe_field));
			field = field->next;
			field->type = tagged_qtype(ctx, type);
			field->count = 1;
		}
		break;
	case STORAGE_ENUM:
	case STORAGE_ERROR:
	case STORAGE_ALIAS:
	case STORAGE_I8:
	case STORAGE_U8:
	case STORAGE_I16:
	case STORAGE_U16:
	case STORAGE_BOOL:
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_RCONST:
	case STORAGE_RUNE:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_I64:
	case STORAGE_U64:
	case STORAGE_ICONST:
	case STORAGE_SIZE:
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
	case STORAGE_NULL:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_VALIST:
	case STORAGE_VOID:
	case STORAGE_FUNCTION:
	case STORAGE_OPAQUE:
	case STORAGE_NEVER:
		abort(); // Invariant
	}

	qbe_append_def(ctx->out, def);
	return &def->type;
}

const struct qbe_type *
qtype_lookup(struct gen_context *ctx,
		const struct type *type,
		bool xtype) {
	switch (type->storage) {
	case STORAGE_U8:
	case STORAGE_I8:
	case STORAGE_BOOL:
		return xtype ? &qbe_byte : &qbe_word;
	case STORAGE_I16:
	case STORAGE_U16:
		return xtype ? &qbe_half : &qbe_word;
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_RUNE:
		return &qbe_word;
	case STORAGE_U64:
	case STORAGE_I64:
		return &qbe_long;
	case STORAGE_SIZE:
		return ctx->arch.sz;
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
	case STORAGE_NULL:
		return ctx->arch.ptr;
	case STORAGE_F32:
		return &qbe_single;
	case STORAGE_F64:
		return &qbe_double;
	case STORAGE_ENUM:
	case STORAGE_ALIAS:
		return qtype_lookup(ctx, type->alias.type, xtype);
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
		return aggregate_lookup(ctx, type);
	case STORAGE_FUNCTION:
		return ctx->arch.ptr;
	case STORAGE_VALIST:
		return ctx->arch.ptr;
	case STORAGE_ERROR:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
	case STORAGE_VOID:
		abort(); // Invariant
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_RCONST:
		return qtype_lookup(ctx, lower_flexible(NULL, type, NULL), xtype);
	}
	abort(); // Invariant
}

bool
type_is_aggregate(const struct type *type)
{
	switch (type->storage) {
	case STORAGE_BOOL:
	case STORAGE_ENUM:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_INT:
	case STORAGE_POINTER:
	case STORAGE_NULL:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VOID:
		return false;
	case STORAGE_FUNCTION:
		// Special case
		return false;
	case STORAGE_ALIAS:
		return type_is_aggregate(type->alias.type);
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
		return true;
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_RCONST:
		lower_flexible(NULL, type, NULL);
		return false;
	case STORAGE_ERROR:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
		assert(0); // Invariant
	}
	assert(0); // Unreachable
}
