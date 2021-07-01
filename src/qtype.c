#include <assert.h>
#include <stdlib.h>
#include "gen.h"
#include "qbe.h"
#include "types.h"
#include "type_store.h"

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
		assert(0); // TODO
	case STORAGE_VOID:
	case STORAGE_FUNCTION:
	case STORAGE_NULL:
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		abort(); // Invariant
	}
	abort(); // Invariant
}
