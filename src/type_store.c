#include <assert.h>
#include <stdlib.h>
#include "type_store.h"
#include "util.h"

const struct type *
builtin_type_for_storage(enum type_storage storage, bool is_const)
{
	switch (storage) {
	case TYPE_STORAGE_BOOL:
		return is_const ? &builtin_type_const_bool : &builtin_type_bool;
	case TYPE_STORAGE_CHAR:
		return is_const ? &builtin_type_const_char : &builtin_type_char;
	case TYPE_STORAGE_F32:
		return is_const ? &builtin_type_const_f32 : &builtin_type_f32;
	case TYPE_STORAGE_F64:
		return is_const ? &builtin_type_const_f64 : &builtin_type_f64;
	case TYPE_STORAGE_I8:
		return is_const ? &builtin_type_const_i8 : &builtin_type_i8;
	case TYPE_STORAGE_I16:
		return is_const ? &builtin_type_const_i16 : &builtin_type_i16;
	case TYPE_STORAGE_I32:
		return is_const ? &builtin_type_const_i32 : &builtin_type_i32;
	case TYPE_STORAGE_I64:
		return is_const ? &builtin_type_const_i64 : &builtin_type_i64;
	case TYPE_STORAGE_INT:
		return is_const ? &builtin_type_const_int : &builtin_type_int;
	case TYPE_STORAGE_RUNE:
		return is_const ? &builtin_type_const_rune : &builtin_type_rune;
	case TYPE_STORAGE_SIZE:
		return is_const ? &builtin_type_const_size : &builtin_type_size;
	case TYPE_STORAGE_U8:
		return is_const ? &builtin_type_const_u8 : &builtin_type_u8;
	case TYPE_STORAGE_U16:
		return is_const ? &builtin_type_const_u16 : &builtin_type_u16;
	case TYPE_STORAGE_U32:
		return is_const ? &builtin_type_const_u32 : &builtin_type_u32;
	case TYPE_STORAGE_U64:
		return is_const ? &builtin_type_const_u64 : &builtin_type_u64;
	case TYPE_STORAGE_UINT:
		return is_const ? &builtin_type_const_uint : &builtin_type_uint;
	case TYPE_STORAGE_UINTPTR:
		return is_const ? &builtin_type_const_uintptr : &builtin_type_uintptr;
	case TYPE_STORAGE_VOID:
		// const void and void are the same type
		return is_const ? &builtin_type_void : &builtin_type_void;
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		return NULL;
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	}
	assert(0); // Unreachable
}

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
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	case TYPE_STORAGE_POINTER:
		hash = djb2(hash, type->pointer.flags);
		hash = atype_hash(store, type->pointer.referent);
		break;
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
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	case TYPE_STORAGE_POINTER:
		hash = djb2(hash, type->pointer.flags);
		hash = type_hash(store, type->pointer.referent);
		break;
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
	return builtin_type_for_storage(atype->storage, is_const);
}

static const struct type *
builtin_for_type(const struct type *type)
{
	bool is_const = (type->flags & TYPE_CONST) != 0;
	return builtin_type_for_storage(type->storage, is_const);
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
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	case TYPE_STORAGE_FUNCTION:
		if (!type_eq_atype(store, type->func.result, atype->func.result)
				|| type->func.variadism != atype->func.variadism
				|| type->func.flags != atype->func.flags) {
			return false;
		}
		struct ast_function_parameters *aparam;
		struct type_func_param *param;
		for (aparam = atype->func.params, param = type->func.params;
				aparam && param;
				aparam = aparam->next, param = param->next) {
			if (!!aparam->next != !!param->next) {
				return false;
			}
			if (!type_eq_atype(store, param->type, aparam->type)) {
				return false;
			}
		}
		return true;
	case TYPE_STORAGE_POINTER:
		return type->pointer.flags == atype->pointer.flags &&
			type_eq_atype(store, type->pointer.referent, atype->pointer.referent);
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	}

	assert(0); // Unreachable
}

static bool
type_eq_type(struct type_store *store,
	const struct type *a, const struct type *b)
{
	assert(0); // TODO
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
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	case TYPE_STORAGE_FUNCTION:
		type->func.result =
			type_store_lookup_atype(store, atype->func.result);
		type->func.variadism = atype->func.variadism;
		type->func.flags = atype->func.flags;
		struct type_func_param *param, **next = &type->func.params;
		for (struct ast_function_parameters *aparam = atype->func.params;
				aparam; aparam = aparam->next) {
			param = *next = calloc(1, sizeof(struct type_func_param));
			param->type = type_store_lookup_atype(store, aparam->type);
			next = &param->next;
		}
		break;
	case TYPE_STORAGE_POINTER:
		type->pointer.flags = atype->pointer.flags;
		type->pointer.referent = type_store_lookup_atype(
			store, atype->pointer.referent);
		break;
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	}
}

static const struct type *type_store_lookup_type(
		struct type_store *store, const struct type *type);

static void
type_init_from_type(struct type_store *store,
	struct type *new, const struct type *old)
{
	new->storage = old->storage;
	new->flags = old->flags;

	switch (old->storage) {
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
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_FUNCTION:
		assert(0); // TODO
	case TYPE_STORAGE_POINTER:
		new->pointer.flags = old->pointer.flags;
		new->pointer.referent = type_store_lookup_type(
			store, old->pointer.referent);
		break;
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

// Used internally for looking up modified forms of other types
static const struct type *
type_store_lookup_type(struct type_store *store, const struct type *type)
{
	const struct type *builtin = builtin_for_type(type);
	if (builtin) {
		return builtin;
	}

	unsigned long hash = type_hash(store, type);
	struct type_bucket **next = &store->buckets[hash % TYPE_STORE_BUCKETS];

	struct type_bucket *bucket;
	while (*next) {
		bucket = *next;
		if (type_eq_type(store, &bucket->type, type)) {
			return &bucket->type;
		}
		next = &bucket->next;
	}

	bucket = *next = calloc(1, sizeof(struct type_bucket));
	type_init_from_type(store, &bucket->type, type);
	return &bucket->type;
}

const struct type *
type_store_lookup_with_flags(struct type_store *store,
	const struct type *type, unsigned int flags)
{
	if ((type->flags & ~flags) == flags) {
		return type;
	}
	struct type new = *type;
	new.flags |= flags;
	return type_store_lookup_type(store, &new);
}

const struct type *
type_store_lookup_pointer(struct type_store *store,
	const struct type *referent, unsigned int ptrflags)
{
	struct type ptr = {
		.storage = TYPE_STORAGE_POINTER,
		.pointer = {
			.referent = referent,
			.flags = ptrflags,
		},
	};
	return type_store_lookup_type(store, &ptr);
}
