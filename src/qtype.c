#include <assert.h>
#include <stdbool.h>
#include "gen.h"
#include "qbe.h"
#include "types.h"

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
		assert(0); // TODO
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
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
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_FUNCTION:
		return qstype_for_type(type);
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	}
	assert(0);
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
			return qtype_for_xtype(qxtype_for_type(type));
		}
		// Fallthrough
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
		return qtype_for_xtype(qstype_for_type(type));
	case TYPE_STORAGE_ARRAY:
		return qtype_for_xtype(Q__AGGREGATE);
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	case TYPE_STORAGE_FUNCTION:
		return qtype_for_xtype(Q__AGGREGATE);
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
		assert(0); // TODO
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_FUNCTION:
		return true;
	}
	assert(0); // Unreachable
}
