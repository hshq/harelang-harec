#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include "gen.h"
#include "qbe.h"
#include "type_store.h"
#include "types.h"
#include "util.h"

enum qbe_stype
qstype_for_type(const struct type *type)
{
	switch (type->storage) {
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
		// Implemented as Q_WORD
	case STORAGE_I16:
	case STORAGE_U16:
		// Implemented as Q_WORD
	case STORAGE_BOOL:
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_RUNE:
	case STORAGE_INT:		// XXX: Architecture dependent
	case STORAGE_UINT:		// XXX: Architecture dependent
		return Q_WORD;
	case STORAGE_I64:
	case STORAGE_U64:
	case STORAGE_SIZE:
	case STORAGE_UINTPTR:	// XXX: Architecture dependent
	case STORAGE_POINTER:	// XXX: Architecture dependent
	case STORAGE_NULL:		// XXX: Architecture dependent
		return Q_LONG;
	case STORAGE_F32:
		return Q_SINGLE;
	case STORAGE_F64:
		return Q_DOUBLE;
	case STORAGE_VOID:
		return Q__VOID;
	case STORAGE_ALIAS:
	case STORAGE_ENUM:
		return qstype_for_type(builtin_type_for_storage(type->_enum.storage, true));
	case STORAGE_ARRAY:
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_FUNCTION:
		assert(0); // Invariant
	}
	assert(0);
}

enum qbe_stype
qxtype_for_type(const struct type *type)
{
	switch (type->storage) {
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
		return Q_BYTE;
	case STORAGE_I16:
	case STORAGE_U16:
		return Q_HALF;
	case STORAGE_BOOL:
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_RUNE:
	case STORAGE_INT:		// XXX: Architecture dependent
	case STORAGE_UINT:		// XXX: Architecture dependent
	case STORAGE_I64:
	case STORAGE_U64:
	case STORAGE_SIZE:
	case STORAGE_UINTPTR:	// XXX: Architecture dependent
	case STORAGE_POINTER:	// XXX: Architecture dependent
	case STORAGE_NULL:		// XXX: Architecture dependent
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_VOID:
	case STORAGE_ALIAS:
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_FUNCTION:
		return qstype_for_type(type);
	case STORAGE_ENUM:
		return qxtype_for_type(builtin_type_for_storage(type->_enum.storage, true));
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		assert(0); // Lowered in check
	}
	assert(0);
}

static const struct qbe_type *
tagged_qtype(struct gen_context *ctx, const struct type *type)
{
	int n = snprintf(NULL, 0, "tags.%zd", ctx->id);
	char *name = xcalloc(1, n + 1);
	snprintf(name, n + 1, "tags.%zd", ctx->id);
	++ctx->id;

	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->kind = Q_TYPE;
	def->name = name;
	def->exported = false;
	def->type.stype = Q__AGGREGATE;
	def->type.base = NULL;
	def->type.name = name;
	def->type.align = SIZE_UNDEFINED;
	def->type.is_union = true;
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
		field->type = qtype_for_type(ctx, tu->type, true);
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

static int
sf_compar(const void *_a, const void *_b)
{
	const struct struct_field **a = (const struct struct_field **)_a;
	const struct struct_field **b = (const struct struct_field **)_b;
	return (int)(*a)->offset - (int)(*b)->offset;
}

static const struct qbe_type *
lookup_aggregate(struct gen_context *ctx, const struct type *type)
{
	for (struct qbe_def *def = ctx->out->defs; def; def = def->next) {
		if (def->kind == Q_TYPE && def->type.base == type) {
			return &def->type;
		}
	}

	switch (type->storage) {
	// Special cases
	case STORAGE_ARRAY:
		return &qbe_long;
	default:
		break;
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
	def->type.align = SIZE_UNDEFINED;
	def->type.size = type->size;

	struct qbe_field *field = &def->type.fields;
	switch (type->storage) {
	case STORAGE_STRING:
		field->type = &qbe_long; // XXX: ARCH
		field->count = 3;
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		if (!type->struct_union.c_compat) {
			def->type.align = type->align;
			field->type = NULL;
			field->count = type->size;
			break;
		}
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
			field->type = qtype_for_type(ctx, tfield->type, true);
			field->count = 1;

			if (i + 1 < n) {
				field->next = xcalloc(1, sizeof(struct qbe_field));
				field = field->next;
			}
		}
		free(tfields);
		break;
	case STORAGE_SLICE:
		field->type = &qbe_long; // XXX: ARCH
		field->count = 3;
		break;
	case STORAGE_TAGGED:
		def->type.align = type->align;
		field->type = &qbe_word; // XXX: ARCH
		field->count = 1;
		if (type->size != builtin_type_uint.size) {
			field->next = xcalloc(1, sizeof(struct qbe_field));
			field = field->next;
			field->type = tagged_qtype(ctx, type);
			field->count = 1;
		}
		break;
	case STORAGE_TUPLE:
		def->type.align = type->align;
		for (const struct type_tuple *tuple = &type->tuple;
				tuple; tuple = tuple->next) {
			field->type = qtype_for_type(ctx, tuple->type, true);
			field->count = 1;

			if (tuple->next) {
				field->next = xcalloc(1, sizeof(struct qbe_field));
				field = field->next;
			}
		}
		break;
	case STORAGE_ENUM:
	case STORAGE_ARRAY:
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
		assert(0); // Invariant
	}

	qbe_append_def(ctx->out, def);
	return &def->type;
}

const struct qbe_type *
qtype_for_type(struct gen_context *ctx, const struct type *type, bool extended)
{
	switch (type->storage) {
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
	case STORAGE_I16:
	case STORAGE_U16:
		if (extended) {
			return qtype_for_xtype(
				qxtype_for_type(type),
				type_is_signed(type));
		}
		// Fallthrough
	case STORAGE_BOOL:
	case STORAGE_ENUM:
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_RUNE:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_I64:
	case STORAGE_U64:
	case STORAGE_SIZE:
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
	case STORAGE_NULL:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_VOID:
		return qtype_for_xtype(qstype_for_type(type), false);
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
		return lookup_aggregate(ctx, type);
	case STORAGE_FUNCTION:
		return qtype_for_xtype(Q__AGGREGATE, false);
	case STORAGE_ALIAS:
		return qtype_for_type(ctx, type->alias.type, extended);
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		assert(0); // Lowered in check
	}
	assert(0); // Unreachable
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
