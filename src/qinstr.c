#include <assert.h>
#include <stdlib.h>
#include "gen.h"
#include "qbe.h"
#include "types.h"
#include "type_store.h"

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
store_for_type(struct gen_context *ctx, const struct type *type)
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
	case STORAGE_SIZE:
		switch (ctx->arch.sz->stype) {
		case Q_LONG:
			return Q_STOREL;
		default:
			assert(0);
		}
		break;
	case STORAGE_POINTER:
	case STORAGE_UINTPTR:
		switch (ctx->arch.ptr->stype) {
		case Q_LONG:
			return Q_STOREL;
		default:
			assert(0);
		}
		break;
	case STORAGE_ENUM:
		return store_for_type(ctx, builtin_type_for_storage(
				type->_enum.storage, false));
	case STORAGE_ALIAS:
		return store_for_type(ctx, type->alias.type);
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

enum qbe_instr
load_for_type(struct gen_context *ctx, const struct type *type)
{
	switch (type->storage) {
	case STORAGE_I8:
		return Q_LOADSB;
	case STORAGE_CHAR:
	case STORAGE_U8:
		return Q_LOADUB;
	case STORAGE_I16:
		return Q_LOADSH;
	case STORAGE_U16:
		return Q_LOADUH;
	case STORAGE_U32:
	case STORAGE_UINT:
	case STORAGE_RUNE:
	case STORAGE_BOOL:
		return Q_LOADUW;
	case STORAGE_I32:
	case STORAGE_INT:
		return Q_LOADSW;
	case STORAGE_I64:
	case STORAGE_U64:
		return Q_LOADL;
	case STORAGE_F32:
		return Q_LOADS;
	case STORAGE_F64:
		return Q_LOADD;
	case STORAGE_SIZE:
		switch (ctx->arch.sz->stype) {
		case Q_LONG:
			return Q_LOADL;
		default:
			assert(0);
		}
		break;
	case STORAGE_POINTER:
	case STORAGE_UINTPTR:
		switch (ctx->arch.ptr->stype) {
		case Q_LONG:
			return Q_LOADL;
		default:
			assert(0);
		}
		break;
	case STORAGE_ENUM:
		return load_for_type(ctx, builtin_type_for_storage(
				type->_enum.storage, false));
	case STORAGE_ALIAS:
		return load_for_type(ctx, type->alias.type);
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
