#include <assert.h>
#include <stdlib.h>
#include "qbe.h"
#include "types.h"

enum qbe_instr
alloc_for_align(size_t align)
{
	switch (align) {
	case 1:
	case 2:
	case 4:
		return Q_ALLOC4;
	case 8:
		return Q_ALLOC8;
	default:
		return Q_ALLOC16;
	}
}

enum qbe_instr
store_for_type(const struct type *type)
{
	switch (type->storage) {
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
		return Q_STOREB;
	case STORAGE_I16:
	case STORAGE_U16:
		return Q_STOREH;
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_RUNE:
	case STORAGE_BOOL:
		return Q_STOREW;
	case STORAGE_I64:
	case STORAGE_U64:
		return Q_STOREL;
	case STORAGE_F32:
		return Q_STORES;
	case STORAGE_F64:
		return Q_STORED;
	case STORAGE_ENUM:
	case STORAGE_POINTER:
	case STORAGE_SIZE:
	case STORAGE_UINTPTR:
		assert(0); // TODO
	case STORAGE_ALIAS:
		return store_for_type(type->alias.type);
	case STORAGE_ARRAY:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_NULL:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VOID:
		abort(); // Invariant
	}
	abort(); // Unreachable
}
