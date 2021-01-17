#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include "types.h"
#include "util.h"

const struct type *
type_dereference(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_ALIAS:
		return type_dereference(type_dealias(type));
	case TYPE_STORAGE_POINTER:
		if (type->pointer.flags & PTR_NULLABLE) {
			return NULL;
		}
		return type_dereference(type->pointer.referent);
	default:
		return type;
	}
}

const struct type *
type_dealias(const struct type *type)
{
	while (type->storage == TYPE_STORAGE_ALIAS) {
		type = type->alias.type;
	}
	return type;
}

const struct struct_field *
type_get_field(const struct type *type, const char *name)
{
	// TODO: We should consider lowering unions into structs with explicit
	// offsets
	assert(type->storage == TYPE_STORAGE_STRUCT
			|| type->storage == TYPE_STORAGE_UNION);
	struct struct_field *field = type->struct_union.fields;
	while (field) {
		if (strcmp(field->name, name) == 0) {
			return field;
		}
		field = field->next;
	}
	return NULL;
}

const char *
type_storage_unparse(enum type_storage storage)
{
	switch (storage) {
	case TYPE_STORAGE_ALIAS:
		return "alias";
	case TYPE_STORAGE_ARRAY:
		return "array";
	case TYPE_STORAGE_BOOL:
		return "bool";
	case TYPE_STORAGE_CHAR:
		return "char";
	case TYPE_STORAGE_ENUM:
		return "enum";
	case TYPE_STORAGE_F32:
		return "f32";
	case TYPE_STORAGE_F64:
		return "f64";
	case TYPE_STORAGE_FUNCTION:
		return "function";
	case TYPE_STORAGE_I16:
		return "i16";
	case TYPE_STORAGE_I32:
		return "i32";
	case TYPE_STORAGE_I64:
		return "i64";
	case TYPE_STORAGE_I8:
		return "i8";
	case TYPE_STORAGE_INT:
		return "int";
	case TYPE_STORAGE_POINTER:
		return "pointer";
	case TYPE_STORAGE_NULL:
		return "rune";
	case TYPE_STORAGE_RUNE:
		return "rune";
	case TYPE_STORAGE_SIZE:
		return "size";
	case TYPE_STORAGE_SLICE:
		return "slice";
	case TYPE_STORAGE_STRING:
		return "str";
	case TYPE_STORAGE_STRUCT:
		return "struct";
	case TYPE_STORAGE_TAGGED_UNION:
		return "tagged union";
	case TYPE_STORAGE_U16:
		return "u16";
	case TYPE_STORAGE_U32:
		return "u32";
	case TYPE_STORAGE_U64:
		return "u64";
	case TYPE_STORAGE_U8:
		return "u8";
	case TYPE_STORAGE_UINT:
		return "uint";
	case TYPE_STORAGE_UINTPTR:
		return "uintptr";
	case TYPE_STORAGE_UNION:
		return "union";
	case TYPE_STORAGE_VOID:
		return "void";
	}
	assert(0);
}

bool
type_is_integer(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		return false;
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
		return true;
	}
	assert(0); // Unreachable
}

bool
type_is_numeric(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_ENUM:
		return false;
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
		return true;
	}
	assert(0); // Unreachable
}

bool
type_is_float(const struct type *type)
{
	return type->storage == TYPE_STORAGE_F32 || type->storage == TYPE_STORAGE_F64;
}

bool
type_is_signed(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
		return false;
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		return true;
	case TYPE_STORAGE_ALIAS:
		return type_is_signed(type_dealias(type));
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	}
	assert(0); // Unreachable
}

uint32_t
type_hash(const struct type *type)
{
	// XXX: ARCH
	uint32_t hash = FNV1A_INIT;
	hash = fnv1a(hash, type->storage);
	hash = fnv1a(hash, type->flags);
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
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_STRING:
		break; // built-ins
	case TYPE_STORAGE_ALIAS:
		for (const struct identifier *ident = &type->alias.ident; ident;
				ident = ident->ns) {
			hash = fnv1a_s(hash, ident->name);
			hash = fnv1a(hash, 0);
		}
		hash = fnv1a_u32(hash, type_hash(type->alias.type));
		break;
	case TYPE_STORAGE_ARRAY:
		hash = fnv1a_u32(hash, type_hash(type->array.members));
		hash = fnv1a_u32(hash, type->array.length);
		break;
	case TYPE_STORAGE_FUNCTION:
		hash = fnv1a_u32(hash, type_hash(type->func.result));
		hash = fnv1a(hash, type->func.variadism);
		hash = fnv1a(hash, type->func.flags);
		for (struct type_func_param *param = type->func.params;
				param; param = param->next) {
			hash = fnv1a_u32(hash, type_hash(param->type));
		}
		break;
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	case TYPE_STORAGE_POINTER:
		hash = fnv1a(hash, type->pointer.flags);
		hash = fnv1a_u32(hash, type_hash(type->pointer.referent));
		break;
	case TYPE_STORAGE_SLICE:
		hash = fnv1a_u32(hash, type_hash(type->array.members));
		break;
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		for (const struct struct_field *field = type->struct_union.fields;
				field; field = field->next) {
			hash = fnv1a_s(hash, field->name);
			hash = fnv1a_u32(hash, type_hash(field->type));
			hash = fnv1a_u32(hash, field->offset);
		}
		break;
	case TYPE_STORAGE_TAGGED_UNION:
		// Invariant: subtypes must be sorted by ID and must not include
		// any other tagged union types, nor any duplicates.
		for (const struct type_tagged_union *tu = &type->tagged;
				tu; tu = tu->next) {
			hash = fnv1a_u32(hash, type_hash(tu->type));
		}
		break;
	}
	return hash;
}

void
builtin_types_init()
{
	struct type *builtins[] = {
		&builtin_type_bool, &builtin_type_char, &builtin_type_f32,
		&builtin_type_f64, &builtin_type_i8, &builtin_type_i16,
		&builtin_type_i32, &builtin_type_i64, &builtin_type_int,
		&builtin_type_u8, &builtin_type_u16, &builtin_type_u32,
		&builtin_type_u64, &builtin_type_uint, &builtin_type_uintptr,
		&builtin_type_null, &builtin_type_rune, &builtin_type_size,
		&builtin_type_void, &builtin_type_const_bool,
		&builtin_type_const_char, &builtin_type_const_f32,
		&builtin_type_const_f64, &builtin_type_const_i8,
		&builtin_type_const_i16, &builtin_type_const_i32,
		&builtin_type_const_i64, &builtin_type_const_int,
		&builtin_type_const_u8, &builtin_type_const_u16,
		&builtin_type_const_u32, &builtin_type_const_u64,
		&builtin_type_const_uint, &builtin_type_const_uintptr,
		&builtin_type_const_rune, &builtin_type_const_size,
		&builtin_type_const_void, &builtin_type_const_ptr_char,
		&builtin_type_str, &builtin_type_const_str,
	};
	for (size_t i = 0; i < sizeof(builtins) / sizeof(builtins[0]); ++i) {
		builtins[i]->id = type_hash(builtins[i]);
	}
}

// Built-in type singletons
struct type builtin_type_bool = {
	.storage = TYPE_STORAGE_BOOL,
	.size = 4, // XXX: ARCH
	.align = 4,
},
builtin_type_char = {
	.storage = TYPE_STORAGE_CHAR,
	.size = 1,
	.align = 1,
},
builtin_type_f32 = {
	.storage = TYPE_STORAGE_F32,
	.size = 4,
	.align = 4,
},
builtin_type_f64 = {
	.storage = TYPE_STORAGE_F64,
	.size = 8,
	.align = 8,
},
builtin_type_i8 = {
	.storage = TYPE_STORAGE_I8,
	.size = 1,
	.align = 1,
},
builtin_type_i16 = {
	.storage = TYPE_STORAGE_I16,
	.size = 2,
	.align = 2,
},
builtin_type_i32 = {
	.storage = TYPE_STORAGE_I32,
	.size = 4,
	.align = 4,
},
builtin_type_i64 = {
	.storage = TYPE_STORAGE_I64,
	.size = 8,
	.align = 8,
},
builtin_type_int = {
	.storage = TYPE_STORAGE_INT,
	.size = 4, // XXX: ARCH
	.align = 4,
},
builtin_type_u8 = {
	.storage = TYPE_STORAGE_U8,
	.size = 1,
	.align = 1,
},
builtin_type_u16 = {
	.storage = TYPE_STORAGE_U16,
	.size = 2,
	.align = 2,
},
builtin_type_u32 = {
	.storage = TYPE_STORAGE_U32,
	.size = 4,
	.align = 4,
},
builtin_type_u64 = {
	.storage = TYPE_STORAGE_U64,
	.size = 8,
	.align = 8,
},
builtin_type_uint = {
	.storage = TYPE_STORAGE_UINT,
	.size = 4,
	.align = 4,
},
builtin_type_uintptr = {
	.storage = TYPE_STORAGE_UINTPTR,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_null = {
	.storage = TYPE_STORAGE_NULL,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_rune = {
	.storage = TYPE_STORAGE_RUNE,
	.size = 4,
	.align = 4,
},
builtin_type_size = {
	.storage = TYPE_STORAGE_SIZE,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_void = {
	.storage = TYPE_STORAGE_VOID,
	.size = 0,
	.align = 0,
},
builtin_type_const_bool = {
	.storage = TYPE_STORAGE_BOOL,
	.flags = TYPE_CONST,
	.size = 4, // XXX: ARCH
	.align = 4,
},
builtin_type_const_char = {
	.storage = TYPE_STORAGE_CHAR,
	.flags = TYPE_CONST,
	.size = 1,
	.align = 1,
},
builtin_type_const_f32 = {
	.storage = TYPE_STORAGE_F32,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_f64 = {
	.storage = TYPE_STORAGE_F64,
	.flags = TYPE_CONST,
	.size = 8,
	.align = 8,
},
builtin_type_const_i8 = {
	.storage = TYPE_STORAGE_I8,
	.flags = TYPE_CONST,
	.size = 1,
	.align = 1,
},
builtin_type_const_i16 = {
	.storage = TYPE_STORAGE_I16,
	.flags = TYPE_CONST,
	.size = 2,
	.align = 2,
},
builtin_type_const_i32 = {
	.storage = TYPE_STORAGE_I32,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_i64 = {
	.storage = TYPE_STORAGE_I64,
	.flags = TYPE_CONST,
	.size = 8,
	.align = 8,
},
builtin_type_const_int = {
	.storage = TYPE_STORAGE_INT,
	.flags = TYPE_CONST,
	.size = 4, // XXX: ARCH
	.align = 4,
},
builtin_type_const_u8 = {
	.storage = TYPE_STORAGE_U8,
	.flags = TYPE_CONST,
	.size = 1,
	.align = 1,
},
builtin_type_const_u16 = {
	.storage = TYPE_STORAGE_U16,
	.flags = TYPE_CONST,
	.size = 2,
	.align = 2,
},
builtin_type_const_u32 = {
	.storage = TYPE_STORAGE_U32,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_u64 = {
	.storage = TYPE_STORAGE_U64,
	.flags = TYPE_CONST,
	.size = 8,
	.align = 8,
},
builtin_type_const_uint = {
	.storage = TYPE_STORAGE_UINT,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_uintptr = {
	.storage = TYPE_STORAGE_UINTPTR,
	.flags = TYPE_CONST,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_const_rune = {
	.storage = TYPE_STORAGE_RUNE,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_size = {
	.storage = TYPE_STORAGE_SIZE,
	.flags = TYPE_CONST,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_const_void = {
	.storage = TYPE_STORAGE_VOID,
	.flags = TYPE_CONST,
	.size = 0,
	.align = 0,
};

// Others
struct type builtin_type_const_ptr_char = {
	.storage = TYPE_STORAGE_POINTER,
	.flags = TYPE_CONST,
	.size = 8, // XXX: ARCH
	.align = 8,
	.pointer = {
		.referent = &builtin_type_char,
	},
},
builtin_type_str = {
	.storage = TYPE_STORAGE_STRING,
	.size = 24, // XXX: ARCH
	.align = 8,
},
builtin_type_const_str = {
	.storage = TYPE_STORAGE_STRING,
	.flags = TYPE_CONST,
	.size = 24, // XXX: ARCH
	.align = 8,
};
