#include <assert.h>
#include <stdbool.h>
#include "types.h"

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
type_is_signed(const struct type *type)
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
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	}
	assert(0); // Unreachable
}

// Built-in type singletons
const struct type builtin_type_bool = {
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
};

// Selected aggregate type singletons
const struct type builtin_type_const_ptr_char = {
	.storage = TYPE_STORAGE_POINTER,
	.flags = TYPE_CONST,
	.size = 8, // XXX: ARCH
	.align = 8,
	.pointer = {
		.referent = &builtin_type_char,
	},
};
