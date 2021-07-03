#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "gen.h"
#include "qbe.h"
#include "types.h"
#include "type_store.h"
#include "util.h"

static int
sf_compar(const void *_a, const void *_b)
{
	const struct struct_field **a = (const struct struct_field **)_a;
	const struct struct_field **b = (const struct struct_field **)_b;
	return (int)(*a)->offset - (int)(*b)->offset;
}

static const struct qbe_type *
aggregate_lookup(struct gen_context *ctx, const struct type *type)
{
	for (struct qbe_def *def = ctx->out->defs; def; def = def->next) {
		if (def->kind == Q_TYPE && def->type.base == type) {
			return &def->type;
		}
	}

	int n = snprintf(NULL, 0, "type.%zd", ctx->id);
	char *name = xcalloc(1, n + 1);
	snprintf(name, n + 1, "type.%zd", ctx->id);
	++ctx->id;

	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->kind = Q_TYPE;
	def->name = name;
	def->type.stype = Q__AGGREGATE;
	def->type.base = type;
	def->type.name = name;

	struct qbe_field *field = &def->type.fields;
	switch (type->storage) {
	case STORAGE_ARRAY:
		assert(type->array.length != SIZE_UNDEFINED);
		field->count = type->array.length;
		field->type = qtype_lookup(ctx, type->array.members, true);
		break;
	case STORAGE_STRING:
		assert(0); // TODO
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		assert(type->struct_union.c_compat); // TODO
		size_t n = 0;
		for (struct struct_field *tfield = type->struct_union.fields;
				tfield; tfield = tfield->next) {
			++n;
		}
		struct struct_field **tfields =
			xcalloc(sizeof(struct struct_field *), n);
		size_t i = 0;
		for (struct struct_field *tfield = type->struct_union.fields;
				tfield; tfield = tfield->next, ++i) {
			tfields[i] = tfield;
		}
		qsort(tfields, n, sizeof(struct struct_field *), sf_compar);
		for (size_t i = 0; i < n; ++i) {
			struct struct_field *tfield = tfields[i];
			field->type = qtype_lookup(ctx, tfield->type, true);
			field->count = 1;

			if (i + 1 < n) {
				field->next = xcalloc(1, sizeof(struct qbe_field));
				field = field->next;
			}
		}
		free(tfields);
		break;
	case STORAGE_SLICE:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		assert(0); // TODO
	case STORAGE_ENUM:
	case STORAGE_ALIAS:
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
	case STORAGE_I16:
	case STORAGE_U16:
	case STORAGE_BOOL:
	case STORAGE_I32:
	case STORAGE_U32:
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
	case STORAGE_VOID:
	case STORAGE_FUNCTION:
		abort(); // Invariant
	}

	qbe_append_def(ctx->out, def);
	return &def->type;
}

const struct qbe_type *qtype_lookup(
		struct gen_context *ctx,
		const struct type *type,
		bool xtype) {
	switch (type->storage) {
	case STORAGE_U8:
	case STORAGE_I8:
	case STORAGE_CHAR:
		return xtype ? &qbe_byte : &qbe_word;
	case STORAGE_I16:
	case STORAGE_U16:
		return xtype ? &qbe_half : &qbe_word;
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_RUNE:
	case STORAGE_BOOL:
		return &qbe_word;
	case STORAGE_U64:
	case STORAGE_I64:
		return &qbe_long;
	case STORAGE_SIZE:
		return ctx->arch.sz;
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
		return ctx->arch.ptr;
	case STORAGE_F32:
		return &qbe_single;
	case STORAGE_F64:
		return &qbe_double;
	case STORAGE_ENUM:
		return qtype_lookup(ctx,
			builtin_type_for_storage(type->_enum.storage, false),
			xtype);
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
	case STORAGE_VOID:
	case STORAGE_FUNCTION:
	case STORAGE_NULL:
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		abort(); // Invariant
	}
	abort(); // Invariant
}

bool
type_is_aggregate(const struct type *type)
{
	switch (type->storage) {
	case STORAGE_BOOL:
	case STORAGE_CHAR:
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
	case STORAGE_ALIAS:
		return type_is_aggregate(type->alias.type);
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_FUNCTION:
		return true;
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		assert(0); // Lowered in check
	}
	assert(0); // Unreachable
}
