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
		assert(type->alias.type != NULL);
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
	case TYPE_STORAGE_TAGGED:
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
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		return false;
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
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
	case TYPE_STORAGE_ALIAS:
		return type_is_integer(type_dealias(type));
	}
	assert(0); // Unreachable
}

bool
type_is_numeric(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
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
	case TYPE_STORAGE_ALIAS:
		return type_is_numeric(type_dealias(type));
	}
	assert(0); // Unreachable
}

bool
type_is_float(const struct type *type)
{
	return type->storage == TYPE_STORAGE_F32 || type->storage == TYPE_STORAGE_F64;
}

static bool
storage_is_signed(enum type_storage storage)
{
	switch (storage) {
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
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
	}
	assert(0); // Unreachable
}

bool
type_is_signed(const struct type *type)
{
	if (type->storage == TYPE_STORAGE_ENUM) {
		return storage_is_signed(type->_enum.storage);
	}
	return storage_is_signed(type_dealias(type)->storage);
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
		hash = fnv1a(hash, type->_enum.storage);
		for (struct type_enum_value *value = type->_enum.values; value;
				value = value->next) {
			hash = fnv1a_s(hash, value->name);
			hash = fnv1a(hash, value->uval);
		}
		break;
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
	case TYPE_STORAGE_TAGGED:
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

// Note that the type this returns is NOT a type singleton and cannot be treated
// as such.
static const struct type *
strip_flags(const struct type *t, struct type *secondary)
{
	if (!t->flags) {
		return t;
	}
	*secondary = *t;
	secondary->flags = 0;
	secondary->id = type_hash(secondary);
	return secondary;
}

static const struct type *
tagged_select_subtype(const struct type *tagged, const struct type *subtype)
{
	tagged = type_dealias(tagged);
	assert(tagged->storage == TYPE_STORAGE_TAGGED);

	size_t nassign = 0;
	const struct type *selected = NULL;
	for (const struct type_tagged_union *tu = &tagged->tagged;
			tu; tu = tu->next) {
		if (tu->type->id == subtype->id) {
			return tu->type;
		}
		if (type_dealias(tu->type)->storage == TYPE_STORAGE_VOID) {
			continue;
		}
		if (type_is_assignable(tu->type, subtype)) {
			selected = tu->type;
			++nassign;
		}
	}

	if (nassign == 1) {
		return selected;
	}

	return NULL;
}

bool
type_is_assignable(const struct type *to, const struct type *from)
{
	// const and non-const types are mutually assignable
	struct type _to, _from;
	to = strip_flags(to, &_to), from = strip_flags(from, &_from);
	if (to->id == from->id) {
		return true;
	}

	struct type _to_secondary, _from_secondary;
	const struct type *to_secondary, *from_secondary;
	switch (to->storage) {
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
		return type_is_integer(from)
			&& type_is_signed(from)
			&& to->size >= from->size;
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
		return type_is_integer(from)
			&& !type_is_signed(from)
			&& to->size >= from->size;
	case TYPE_STORAGE_UINTPTR:
		return (type_is_integer(from)
				&& !type_is_signed(from)
				&& to->size >= from->size)
			|| from->storage == TYPE_STORAGE_POINTER;
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		return type_is_float(from);
	case TYPE_STORAGE_POINTER:
		to_secondary = strip_flags(to->pointer.referent, &_to_secondary);
		switch (from->storage) {
		case TYPE_STORAGE_UINTPTR:
			return true;
		case TYPE_STORAGE_NULL:
			return to->pointer.flags & PTR_NULLABLE;
		case TYPE_STORAGE_POINTER:
			from_secondary = strip_flags(
				from->pointer.referent, &_from_secondary);
			switch (to_secondary->storage) {
			case TYPE_STORAGE_VOID:
				return true;
			case TYPE_STORAGE_ARRAY:
				if (type_is_assignable(to_secondary, from_secondary)) {
					return true;
				}
				break;
			default:
				if (to_secondary != from_secondary) {
					return false;
				}
				break;
			}
			if (from->pointer.flags & PTR_NULLABLE) {
				return to->pointer.flags & PTR_NULLABLE;
			}
			return true;
		case TYPE_STORAGE_STRING:
			return to->pointer.referent->storage == TYPE_STORAGE_CHAR
				&& to->pointer.referent->flags & TYPE_CONST;
		default:
			return false;
		}
		assert(0); // Unreachable
	case TYPE_STORAGE_ALIAS:
		return type_is_assignable(to->alias.type, from);
	case TYPE_STORAGE_STRING:
		return to->id == builtin_type_const_ptr_char.id;
	case TYPE_STORAGE_VOID:
		return true;
	case TYPE_STORAGE_SLICE:
		to_secondary = strip_flags(to->array.members, &_to_secondary);
		from_secondary = strip_flags(from->array.members, &_from_secondary);
		return (from->storage == TYPE_STORAGE_ARRAY || from->storage == TYPE_STORAGE_SLICE)
			&& to_secondary->id == from_secondary->id;
	case TYPE_STORAGE_ARRAY:
		return from->storage == TYPE_STORAGE_ARRAY
			&& to->array.length == SIZE_UNDEFINED
			&& from->array.length != SIZE_UNDEFINED;
	case TYPE_STORAGE_TAGGED:
		// XXX: Needs work!
		return tagged_select_subtype(to, from) != NULL || true;
	// The following types are only assignable from themselves, and are
	// handled above:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		return false;
	}

	assert(0); // Unreachable
}

static bool
castable_to_tagged(const struct type *to, const struct type *from)
{
	if (type_dealias(from)->storage == TYPE_STORAGE_TAGGED) {
		return true;
	}

	size_t ncastable = 0;
	to = type_dealias(to);
	for (const struct type_tagged_union *tu = &to->tagged;
			tu; tu = tu->next) {
		if (tu->type->id == from->id) {
			return true;
		}
		if (type_is_castable(tu->type, from)) {
			++ncastable;
		}
	}

	return ncastable == 1;
}

static bool
castable_from_tagged(const struct type *to, const struct type *from)
{
	if (type_dealias(to)->storage == TYPE_STORAGE_TAGGED) {
		return true;
	}

	size_t ncastable = 0;
	from = type_dealias(from);
	for (const struct type_tagged_union *tu = &from->tagged;
			tu; tu = tu->next) {
		if (tu->type->id == to->id) {
			return true;
		}
		if (type_is_castable(tu->type, to)) {
			++ncastable;
		}
	}

	return ncastable == 1;
}

bool
type_is_castable(const struct type *to, const struct type *from)
{
	if (type_dealias(to)->storage == TYPE_STORAGE_TAGGED) {
		return castable_to_tagged(to, from);
	} else if (type_dealias(from)->storage == TYPE_STORAGE_TAGGED) {
		return castable_from_tagged(to, from);
	}

	to = type_dealias(to), from = type_dealias(from);
	if (to == from) {
		return true;
	}

	switch (to->storage) {
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
		return from->storage == TYPE_STORAGE_ENUM || type_is_numeric(from);
	case TYPE_STORAGE_U8:
		return from->storage == TYPE_STORAGE_ENUM
			|| type_is_numeric(from)
			|| from->storage == TYPE_STORAGE_CHAR;
	case TYPE_STORAGE_U32:
		return from->storage == TYPE_STORAGE_ENUM
			|| type_is_numeric(from)
			|| from->storage == TYPE_STORAGE_RUNE;
	case TYPE_STORAGE_CHAR:
		return from->storage == TYPE_STORAGE_U8;
	case TYPE_STORAGE_RUNE:
		return from->storage == TYPE_STORAGE_RUNE;
	case TYPE_STORAGE_ENUM:
		return from->storage == TYPE_STORAGE_ENUM || type_is_integer(from);
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		return type_is_numeric(from);
	case TYPE_STORAGE_UINTPTR:
		return from->storage == TYPE_STORAGE_POINTER
			|| from->storage == TYPE_STORAGE_NULL
			|| type_is_numeric(from);
	case TYPE_STORAGE_POINTER:
		if (from->storage == TYPE_STORAGE_STRING
				&& to->pointer.referent->storage == TYPE_STORAGE_CHAR
				&& to->pointer.referent->flags & TYPE_CONST) {
			return true;
		}
		return from->storage == TYPE_STORAGE_POINTER
			|| from->storage == TYPE_STORAGE_NULL
			|| from->storage == TYPE_STORAGE_UINTPTR;
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_ARRAY:
		return from->storage == TYPE_STORAGE_SLICE
			|| from->storage == TYPE_STORAGE_ARRAY;
	// Cannot be cast:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_NULL:
		return false;
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_ALIAS:
		assert(0); // Handled above
	}

	assert(0); // Unreachable
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
