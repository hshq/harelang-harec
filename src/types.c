#include <assert.h>
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
