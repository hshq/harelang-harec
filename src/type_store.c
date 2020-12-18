#include <assert.h>
#include <stdlib.h>
#include "type_store.h"
#include "util.h"

unsigned long
atype_hash(struct type_store *store, const struct ast_type *type)
{
	unsigned long hash = DJB2_INIT;
	hash = djb2(hash, type->storage);
	hash = djb2(hash, type->flags);
	switch (type->storage) {
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
		break; // built-ins
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
		hash = djb2(hash, atype_hash(store, type->func.result));
		hash = djb2(hash, type->func.variadism);
		hash = djb2(hash, type->func.flags);
		for (struct ast_function_parameters *param = type->func.params;
				param; param = param->next) {
			hash = djb2(hash, atype_hash(store, param->type));
		}
		break;
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	}
	return hash;
}

unsigned long
type_hash(struct type_store *store, const struct type *type)
{
	unsigned long hash = DJB2_INIT;
	hash = djb2(hash, type->storage);
	hash = djb2(hash, type->flags);
	switch (type->storage) {
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
		break; // built-ins
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
		hash = djb2(hash, type_hash(store, type->func.result));
		hash = djb2(hash, type->func.variadism);
		hash = djb2(hash, type->func.flags);
		for (struct type_func_param *param = type->func.params;
				param; param = param->next) {
			hash = djb2(hash, type_hash(store, param->type));
		}
		break;
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	}
	return hash;
}

static const struct type *
builtin_for_atype(const struct ast_type *atype)
{
	bool is_const = (atype->flags & TYPE_CONST) != 0;
	switch (atype->storage) {
	case TYPE_STORAGE_BOOL:
		return is_const ? &builtin_type_bool : &builtin_type_const_bool;
	case TYPE_STORAGE_CHAR:
		return is_const ? &builtin_type_char : &builtin_type_const_char;
	case TYPE_STORAGE_F32:
		return is_const ? &builtin_type_f32 : &builtin_type_const_f32;
	case TYPE_STORAGE_F64:
		return is_const ? &builtin_type_f64 : &builtin_type_const_f64;
	case TYPE_STORAGE_I8:
		return is_const ? &builtin_type_i8 : &builtin_type_const_i8;
	case TYPE_STORAGE_I16:
		return is_const ? &builtin_type_i16 : &builtin_type_const_i16;
	case TYPE_STORAGE_I32:
		return is_const ? &builtin_type_i32 : &builtin_type_const_i32;
	case TYPE_STORAGE_I64:
		return is_const ? &builtin_type_i64 : &builtin_type_const_i64;
	case TYPE_STORAGE_INT:
		return is_const ? &builtin_type_int : &builtin_type_const_int;
	case TYPE_STORAGE_RUNE:
		return is_const ? &builtin_type_rune : &builtin_type_const_rune;
	case TYPE_STORAGE_SIZE:
		return is_const ? &builtin_type_size : &builtin_type_const_size;
	case TYPE_STORAGE_U8:
		return is_const ? &builtin_type_u8 : &builtin_type_const_u8;
	case TYPE_STORAGE_U16:
		return is_const ? &builtin_type_u16 : &builtin_type_const_u16;
	case TYPE_STORAGE_U32:
		return is_const ? &builtin_type_u32 : &builtin_type_const_u32;
	case TYPE_STORAGE_U64:
		return is_const ? &builtin_type_u64 : &builtin_type_const_u64;
	case TYPE_STORAGE_UINT:
		return is_const ? &builtin_type_uint : &builtin_type_const_uint;
	case TYPE_STORAGE_UINTPTR:
		return is_const ? &builtin_type_uintptr : &builtin_type_const_uintptr;
	case TYPE_STORAGE_VOID:
		return is_const ? &builtin_type_void : &builtin_type_const_void;
	case TYPE_STORAGE_POINTER:
		if (atype->pointer.referent->storage == TYPE_STORAGE_CHAR
				&& atype->pointer.referent->flags == TYPE_CONST) {
			return &builtin_type_const_ptr_char;
		}
		return NULL;
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		return NULL;

	}
	assert(0); // Unreachable
}

static bool
type_eq_atype(struct type_store *store,
	const struct type *type,
	const struct ast_type *atype)
{
	if (type->storage != atype->storage || type->flags != atype->flags) {
		return false;
	}

	switch (type->storage) {
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
		return true;
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	}

	assert(0); // Unreachable
}

static void
type_init_from_atype(struct type_store *store,
	struct type *type,
	const struct ast_type *atype)
{
	type->storage = atype->storage;
	type->flags = atype->flags;

	switch (type->storage) {
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
		assert(0); // Invariant
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
		assert(0); // TODO
	case TYPE_STORAGE_FUNCTION:
		type->func.result =
			type_store_lookup_atype(store, atype->func.result);
		type->func.variadism = atype->func.variadism;
		type->func.flags = atype->func.flags;
		assert(!atype->func.params); // TODO
		break;
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	}
}

const struct type *
type_store_lookup_atype(struct type_store *store, const struct ast_type *atype)
{
	const struct type *builtin = builtin_for_atype(atype);
	if (builtin) {
		return builtin;
	}

	unsigned long hash = atype_hash(store, atype);
	struct type_bucket **next = &store->buckets[hash % TYPE_STORE_BUCKETS];

	struct type_bucket *bucket;
	while (*next) {
		bucket = *next;
		if (type_eq_atype(store, &bucket->type, atype)) {
			return &bucket->type;
		}
		next = &bucket->next;
	}

	bucket = *next = calloc(1, sizeof(struct type_bucket));
	type_init_from_atype(store, &bucket->type, atype);
	return &bucket->type;
}
