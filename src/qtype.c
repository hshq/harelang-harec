#include <assert.h>
#include <stdbool.h>
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
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_U8:
		// Implemented as Q_WORD
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
		// Implemented as Q_WORD
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_INT:		// XXX: Architecture dependent
	case TYPE_STORAGE_UINT:		// XXX: Architecture dependent
		return Q_WORD;
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_UINTPTR:	// XXX: Architecture dependent
	case TYPE_STORAGE_POINTER:	// XXX: Architecture dependent
	case TYPE_STORAGE_NULL:		// XXX: Architecture dependent
		return Q_LONG;
	case TYPE_STORAGE_F32:
		return Q_SINGLE;
	case TYPE_STORAGE_F64:
		return Q_DOUBLE;
	case TYPE_STORAGE_VOID:
		return Q__VOID;
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ENUM:
		return qstype_for_type(builtin_type_for_storage(type->_enum.storage, true));
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_FUNCTION:
		assert(0); // Invariant
	}
	assert(0);
}

enum qbe_stype
qxtype_for_type(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_U8:
		return Q_BYTE;
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
		return Q_HALF;
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_INT:		// XXX: Architecture dependent
	case TYPE_STORAGE_UINT:		// XXX: Architecture dependent
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_UINTPTR:	// XXX: Architecture dependent
	case TYPE_STORAGE_POINTER:	// XXX: Architecture dependent
	case TYPE_STORAGE_NULL:		// XXX: Architecture dependent
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_FUNCTION:
		return qstype_for_type(type);
	case TYPE_STORAGE_ENUM:
		return qxtype_for_type(builtin_type_for_storage(type->_enum.storage, true));
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
	for (const struct type_tagged_union *tu = &type->tagged;
			tu; tu = tu->next) {
		if (tu->type->size == 0) {
			continue;
		}
		field->type = qtype_for_type(ctx, tu->type, true);
		field->count = 1;
		if (tu->next && tu->next->type->size != 0) {
			field->next = xcalloc(1, sizeof(struct qbe_field));
			field = field->next;
		}
	}

	qbe_append_def(ctx->out, def);
	return &def->type;
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
	case TYPE_STORAGE_ARRAY:
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
	case TYPE_STORAGE_STRING:
		field->type = &qbe_long; // XXX: ARCH
		field->count = 3;
		break;
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		assert(type->struct_union.c_compat); // TODO
		for (struct struct_field *tfield = type->struct_union.fields;
				tfield; tfield = tfield->next) {
			field->type = qtype_for_type(ctx, tfield->type, true);
			field->count = 1;

			if (tfield->next) {
				field->next = xcalloc(1, sizeof(struct qbe_field));
				field = field->next;
			}
		}
		break;
	case TYPE_STORAGE_SLICE:
		field->type = &qbe_long; // XXX: ARCH
		field->count = 3;
		break;
	case TYPE_STORAGE_TAGGED:
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
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_FUNCTION:
		assert(0); // Invariant
	}

	qbe_append_def(ctx->out, def);
	return &def->type;
}

const struct qbe_type *
qtype_for_type(struct gen_context *ctx, const struct type *type, bool extended)
{
	switch (type->storage) {
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
		if (extended) {
			return qtype_for_xtype(
				qxtype_for_type(type),
				type_is_signed(type));
		}
		// Fallthrough
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_VOID:
		return qtype_for_xtype(qstype_for_type(type), false);
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_UNION:
		return lookup_aggregate(ctx, type);
	case TYPE_STORAGE_FUNCTION:
		return qtype_for_xtype(Q__AGGREGATE, false);
	case TYPE_STORAGE_ALIAS:
		return qtype_for_type(ctx, type->alias.type, extended);
	}
	assert(0); // Unreachable
}

bool
type_is_aggregate(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
		return false;
	case TYPE_STORAGE_ALIAS:
		return type_is_aggregate(type->alias.type);
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_FUNCTION:
		return true;
	}
	assert(0); // Unreachable
}
