#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "types.h"
#include "util.h"

const struct type *
type_dereference(const struct type *type)
{
	switch (type->storage) {
	case STORAGE_ALIAS:
		return type_dereference(type_dealias(type));
	case STORAGE_POINTER:
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
	while (type->storage == STORAGE_ALIAS) {
		if (type->alias.type == NULL) {
			fprintf(stderr, "Cannot dealias incomplete type %s\n",
				identifier_unparse(&type->alias.ident));
			assert(0);
		}
		type = type->alias.type;
	}
	return type;
}

const struct struct_field *
type_get_field(const struct type *type, const char *name)
{
	// TODO: We should consider lowering unions into structs with explicit
	// offsets
	assert(type->storage == STORAGE_STRUCT
			|| type->storage == STORAGE_UNION);
	struct struct_field *field = type->struct_union.fields;
	while (field) {
		if (strcmp(field->name, name) == 0) {
			return field;
		}
		field = field->next;
	}
	return NULL;
}

const struct type_tuple *
type_get_value(const struct type *type, uintmax_t index)
{
	assert(type->storage == STORAGE_TUPLE);
	const struct type_tuple *tuple = &type->tuple;
	while (tuple) {
		if (index == 0) {
			return tuple;
		}
		tuple = tuple->next;
		--index;
	}
	return NULL;
}

const char *
type_storage_unparse(enum type_storage storage)
{
	switch (storage) {
	case STORAGE_ALIAS:
		return "alias";
	case STORAGE_ARRAY:
		return "array";
	case STORAGE_BOOL:
		return "bool";
	case STORAGE_CHAR:
		return "char";
	case STORAGE_ENUM:
		return "enum";
	case STORAGE_F32:
		return "f32";
	case STORAGE_F64:
		return "f64";
	case STORAGE_FCONST:
		return "fconst";
	case STORAGE_FUNCTION:
		return "function";
	case STORAGE_I16:
		return "i16";
	case STORAGE_I32:
		return "i32";
	case STORAGE_I64:
		return "i64";
	case STORAGE_I8:
		return "i8";
	case STORAGE_ICONST:
		return "iconst";
	case STORAGE_INT:
		return "int";
	case STORAGE_POINTER:
		return "pointer";
	case STORAGE_NULL:
		return "rune";
	case STORAGE_RUNE:
		return "rune";
	case STORAGE_SIZE:
		return "size";
	case STORAGE_SLICE:
		return "slice";
	case STORAGE_STRING:
		return "str";
	case STORAGE_STRUCT:
		return "struct";
	case STORAGE_TAGGED:
		return "tagged union";
	case STORAGE_TUPLE:
		return "tuple";
	case STORAGE_U16:
		return "u16";
	case STORAGE_U32:
		return "u32";
	case STORAGE_U64:
		return "u64";
	case STORAGE_U8:
		return "u8";
	case STORAGE_UINT:
		return "uint";
	case STORAGE_UINTPTR:
		return "uintptr";
	case STORAGE_UNION:
		return "union";
	case STORAGE_VOID:
		return "void";
	}
	assert(0);
}

bool
type_is_integer(const struct type *type)
{
	switch (type->storage) {
	case STORAGE_VOID:
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_BOOL:
	case STORAGE_NULL:
	case STORAGE_RUNE:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		return false;
	case STORAGE_CHAR:
	case STORAGE_ENUM:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
		return true;
	case STORAGE_ALIAS:
		return type_is_integer(type_dealias(type));
	}
	assert(0); // Unreachable
}

bool
type_is_numeric(const struct type *type)
{
	switch (type->storage) {
	case STORAGE_VOID:
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_RUNE:
	case STORAGE_NULL:
	case STORAGE_ENUM:
		return false;
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
		return true;
	case STORAGE_ALIAS:
		return type_is_numeric(type_dealias(type));
	}
	assert(0); // Unreachable
}

bool
type_is_float(const struct type *type)
{
	return type->storage == STORAGE_F32 || type->storage == STORAGE_F64;
}

bool
type_storage_is_signed(enum type_storage storage)
{
	switch (storage) {
	case STORAGE_VOID:
	case STORAGE_ALIAS:
	case STORAGE_ARRAY:
	case STORAGE_ENUM:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_RUNE:
	case STORAGE_NULL:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
		return false;
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		return true;
	case STORAGE_ICONST:
		assert(0); // XXX
	}
	assert(0); // Unreachable
}

bool
type_is_signed(const struct type *type)
{
	if (type->storage == STORAGE_ENUM) {
		return type_storage_is_signed(type->_enum.storage);
	}
	return type_storage_is_signed(type_dealias(type)->storage);
}

bool storage_is_flexible(enum type_storage storage)
{
	switch (storage) {
	case STORAGE_ALIAS:
	case STORAGE_ARRAY:
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_ENUM:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FUNCTION:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_INT:
	case STORAGE_NULL:
	case STORAGE_POINTER:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_UNION:
	case STORAGE_VOID:
		return false;
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		return true;
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
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_NULL:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VOID:
	case STORAGE_STRING:
		break; // built-ins
	case STORAGE_ALIAS:
		for (const struct identifier *ident = &type->alias.ident; ident;
				ident = ident->ns) {
			hash = fnv1a_s(hash, ident->name);
			hash = fnv1a(hash, 0);
		}
		break;
	case STORAGE_ARRAY:
		hash = fnv1a_u32(hash, type_hash(type->array.members));
		hash = fnv1a_u32(hash, type->array.length);
		break;
	case STORAGE_FUNCTION:
		hash = fnv1a_u32(hash, type_hash(type->func.result));
		hash = fnv1a(hash, type->func.variadism);
		hash = fnv1a(hash, type->func.flags);
		for (struct type_func_param *param = type->func.params;
				param; param = param->next) {
			hash = fnv1a_u32(hash, type_hash(param->type));
		}
		break;
	case STORAGE_ENUM:
		hash = fnv1a(hash, type->_enum.storage);
		for (struct type_enum_value *value = type->_enum.values; value;
				value = value->next) {
			hash = fnv1a_s(hash, value->name);
			hash = fnv1a(hash, value->uval);
		}
		break;
	case STORAGE_POINTER:
		hash = fnv1a(hash, type->pointer.flags);
		hash = fnv1a_u32(hash, type_hash(type->pointer.referent));
		break;
	case STORAGE_SLICE:
		hash = fnv1a_u32(hash, type_hash(type->array.members));
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		for (const struct struct_field *field = type->struct_union.fields;
				field; field = field->next) {
			hash = fnv1a_s(hash, field->name);
			hash = fnv1a_u32(hash, type_hash(field->type));
			hash = fnv1a_u32(hash, field->offset);
		}
		break;
	case STORAGE_TAGGED:
		// Invariant: subtypes must be sorted by ID and must not include
		// any other tagged union types, nor any duplicates.
		for (const struct type_tagged_union *tu = &type->tagged;
				tu; tu = tu->next) {
			hash = fnv1a_u32(hash, type_hash(tu->type));
		}
		break;
	case STORAGE_TUPLE:
		for (const struct type_tuple *tuple = &type->tuple;
				tuple; tuple = tuple->next) {
			hash = fnv1a_u32(hash, type_hash(tuple->type));
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

const struct type *
tagged_select_subtype(const struct type *tagged, const struct type *subtype)
{
	tagged = type_dealias(tagged);
	assert(tagged->storage == STORAGE_TAGGED);

	struct type _stripped;
	const struct type *stripped = strip_flags(subtype, &_stripped);

	size_t nassign = 0;
	const struct type *selected = NULL;
	for (const struct type_tagged_union *tu = &tagged->tagged;
			tu; tu = tu->next) {
		struct type _tustripped;
		const struct type *tustripped =
			strip_flags(tu->type, &_tustripped);
		// XXX: Kind of stupid
		if (tu->type->id == subtype->id
				|| tu->type->id == stripped->id
				|| tustripped->id == subtype->id
				|| tustripped->id == stripped->id) {
			return tu->type;
		}

		if (type_dealias(tu->type)->storage == STORAGE_VOID) {
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
tagged_subset_compat(const struct type *to, const struct type *from)
{
	// Note: this implementation depends on the invariant that tagged union
	// member types are sorted by their type ID.
	to = type_dealias(to), from = type_dealias(from);
	if (to->storage != STORAGE_TAGGED || from->storage != STORAGE_TAGGED) {
		return false;
	}
	const struct type_tagged_union *to_tu = &to->tagged,
	      *from_tu = &from->tagged;
	while (from_tu && to_tu) {
		while (to_tu) {
			if (to_tu->type->id == from_tu->type->id) {
				from_tu = from_tu->next;
				to_tu = to_tu->next;
				break;
			}
			to_tu = to_tu->next;
		}
	}

	return !from_tu;
}

bool
type_is_assignable(const struct type *to, const struct type *from)
{
	if (type_dealias(to)->storage != STORAGE_TAGGED) {
		to = type_dealias(to);
		from = type_dealias(from);
	}

	// const and non-const types are mutually assignable
	struct type _to, _from;
	const struct type *from_orig = from;
	to = strip_flags(to, &_to), from = strip_flags(from, &_from);
	if (to->id == from->id) {
		return true;
	}

	struct type _to_secondary, _from_secondary;
	const struct type *to_secondary, *from_secondary;
	switch (to->storage) {
	case STORAGE_ICONST:
	case STORAGE_FCONST:
		assert(0); // Invariant
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
		return type_is_integer(from)
			&& type_is_signed(from)
			&& to->size >= from->size;
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
		return type_is_integer(from)
			&& !type_is_signed(from)
			&& to->size >= from->size;
	case STORAGE_F32:
	case STORAGE_F64:
		return type_is_float(from);
	case STORAGE_POINTER:
		to_secondary = type_dealias(to->pointer.referent);
		to_secondary = strip_flags(to_secondary, &_to_secondary);
		switch (from->storage) {
		case STORAGE_UINTPTR:
			return true;
		case STORAGE_NULL:
			return to->pointer.flags & PTR_NULLABLE;
		case STORAGE_POINTER:
			from_secondary = type_dealias(from->pointer.referent);
			from_secondary = strip_flags(from_secondary, &_from_secondary);
			switch (to_secondary->storage) {
			case STORAGE_VOID:
				return true;
			case STORAGE_ARRAY:
				if (type_is_assignable(to_secondary, from_secondary)) {
					return true;
				}
				break;
			default:
				if (to_secondary->id != from_secondary->id) {
					return false;
				}
				break;
			}
			if (from->pointer.flags & PTR_NULLABLE) {
				return to->pointer.flags & PTR_NULLABLE;
			}
			return true;
		case STORAGE_STRING:
			return to->pointer.referent->storage == STORAGE_CHAR
				&& to->pointer.referent->flags & TYPE_CONST;
		default:
			return false;
		}
		assert(0); // Unreachable
	case STORAGE_ALIAS:
		assert(to->alias.type);
		return type_is_assignable(to->alias.type, from);
	case STORAGE_STRING:
		return to->id == builtin_type_ptr_const_char.id;
	case STORAGE_VOID:
		return true;
	case STORAGE_SLICE:
		from = type_dealias(from);
		if (from->storage != STORAGE_ARRAY
				&& from->storage != STORAGE_SLICE) {
			return false;
		}
		to_secondary = strip_flags(
			type_dealias(to->array.members),
			&_to_secondary);
		from_secondary = strip_flags(
			type_dealias(from->array.members),
			&_from_secondary);
		if (to->storage == STORAGE_SLICE
				&& to_secondary->storage == STORAGE_VOID) {
			return true;
		}
		return to_secondary->id == from_secondary->id;
	case STORAGE_ARRAY:
		return from->storage == STORAGE_ARRAY
			&& to->array.length == SIZE_UNDEFINED
			&& from->array.length != SIZE_UNDEFINED;
	case STORAGE_TAGGED:
		return tagged_select_subtype(to, from_orig) != NULL
			|| tagged_subset_compat(to, from);
	// The following types are only assignable from themselves, and are
	// handled above:
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_ENUM:
	case STORAGE_FUNCTION:
	case STORAGE_NULL:
	case STORAGE_RUNE:
	case STORAGE_STRUCT:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_UINTPTR:
		return false;
	}

	assert(0); // Unreachable
}

static bool
castable_from_tagged(const struct type *to, const struct type *from)
{
	// TODO: This may need to be expanded upon
	from = type_dealias(from);
	for (const struct type_tagged_union *tu = &from->tagged;
			tu; tu = tu->next) {
		if (tu->type->id == to->id) {
			return true;
		}
	}
	return false;
}

bool
type_is_castable(const struct type *to, const struct type *from)
{
	if (type_dealias(to)->storage == STORAGE_TAGGED) {
		return tagged_select_subtype(to, from) != NULL
			|| tagged_subset_compat(to, from);
	}

	if (type_dealias(from)->storage == STORAGE_TAGGED) {
		return castable_from_tagged(to, from);
	}

	to = type_dealias(to), from = type_dealias(from);
	if (to == from) {
		return true;
	}

	switch (to->storage) {
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		assert(0); // TODO
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
	case STORAGE_SIZE:
	case STORAGE_U16:
	case STORAGE_U64:
	case STORAGE_UINT:
		return from->storage == STORAGE_ENUM || type_is_numeric(from);
	case STORAGE_U8:
		return from->storage == STORAGE_ENUM
			|| type_is_numeric(from)
			|| from->storage == STORAGE_CHAR;
	case STORAGE_U32:
		return from->storage == STORAGE_ENUM
			|| type_is_numeric(from)
			|| from->storage == STORAGE_RUNE;
	case STORAGE_CHAR:
		return from->storage == STORAGE_U8;
	case STORAGE_RUNE:
		return from->storage == STORAGE_U32;
	case STORAGE_ENUM:
		return from->storage == STORAGE_ENUM || type_is_integer(from);
	case STORAGE_F32:
	case STORAGE_F64:
		return type_is_numeric(from);
	case STORAGE_UINTPTR:
		return from->storage == STORAGE_POINTER
			|| from->storage == STORAGE_NULL
			|| type_is_numeric(from);
	case STORAGE_POINTER:
		if (from->storage == STORAGE_STRING
				&& to->pointer.referent->storage == STORAGE_CHAR
				&& to->pointer.referent->flags & TYPE_CONST) {
			return true;
		}
		return from->storage == STORAGE_POINTER
			|| from->storage == STORAGE_NULL
			|| from->storage == STORAGE_UINTPTR
			|| (to->pointer.referent->storage == STORAGE_ARRAY
					&& from->storage == STORAGE_SLICE);
	case STORAGE_SLICE:
	case STORAGE_ARRAY:
		return from->storage == STORAGE_SLICE
			|| from->storage == STORAGE_ARRAY;
	// Cannot be cast:
	case STORAGE_BOOL:
	case STORAGE_VOID:
	case STORAGE_FUNCTION:
	case STORAGE_TUPLE:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
	case STORAGE_STRING:
	case STORAGE_NULL:
		return false;
	case STORAGE_TAGGED:
	case STORAGE_ALIAS:
		assert(0); // Handled above
	}

	assert(0); // Unreachable
}

void
builtin_types_init()
{
	struct type *builtins[] = {
		&builtin_type_bool, &builtin_type_char, &builtin_type_f32,
		&builtin_type_f64, &builtin_type_fconst, &builtin_type_i8,
		&builtin_type_i16, &builtin_type_i32, &builtin_type_i64,
		&builtin_type_iconst, &builtin_type_int, &builtin_type_u8,
		&builtin_type_u16, &builtin_type_u32, &builtin_type_u64,
		&builtin_type_uint, &builtin_type_uintptr, &builtin_type_null,
		&builtin_type_rune, &builtin_type_size, &builtin_type_void,
		&builtin_type_const_bool, &builtin_type_const_char,
		&builtin_type_const_f32, &builtin_type_const_f64,
		&builtin_type_const_fconst, &builtin_type_const_i8,
		&builtin_type_const_i16, &builtin_type_const_i32,
		&builtin_type_const_i64, &builtin_type_const_iconst,
		&builtin_type_const_int, &builtin_type_const_u8,
		&builtin_type_const_u16, &builtin_type_const_u32,
		&builtin_type_const_u64, &builtin_type_const_uint,
		&builtin_type_const_uintptr, &builtin_type_const_rune,
		&builtin_type_const_size, &builtin_type_const_void,
		&builtin_type_ptr_const_char, &builtin_type_str,
		&builtin_type_const_str,
	};
	for (size_t i = 0; i < sizeof(builtins) / sizeof(builtins[0]); ++i) {
		builtins[i]->id = type_hash(builtins[i]);
	}
}

// Built-in type singletons
struct type builtin_type_bool = {
	.storage = STORAGE_BOOL,
	.size = 4, // XXX: ARCH
	.align = 4,
},
builtin_type_char = {
	.storage = STORAGE_CHAR,
	.size = 1,
	.align = 1,
},
builtin_type_f32 = {
	.storage = STORAGE_F32,
	.size = 4,
	.align = 4,
},
builtin_type_f64 = {
	.storage = STORAGE_F64,
	.size = 8,
	.align = 8,
},
builtin_type_fconst = {
	.storage = STORAGE_FCONST,
	.size = SIZE_UNDEFINED,
	.align = ALIGN_UNDEFINED,
},
builtin_type_i8 = {
	.storage = STORAGE_I8,
	.size = 1,
	.align = 1,
},
builtin_type_i16 = {
	.storage = STORAGE_I16,
	.size = 2,
	.align = 2,
},
builtin_type_i32 = {
	.storage = STORAGE_I32,
	.size = 4,
	.align = 4,
},
builtin_type_i64 = {
	.storage = STORAGE_I64,
	.size = 8,
	.align = 8,
},
builtin_type_iconst = {
	.storage = STORAGE_ICONST,
	.size = SIZE_UNDEFINED,
	.align = ALIGN_UNDEFINED,
},
builtin_type_int = {
	.storage = STORAGE_INT,
	.size = 4, // XXX: ARCH
	.align = 4,
},
builtin_type_u8 = {
	.storage = STORAGE_U8,
	.size = 1,
	.align = 1,
},
builtin_type_u16 = {
	.storage = STORAGE_U16,
	.size = 2,
	.align = 2,
},
builtin_type_u32 = {
	.storage = STORAGE_U32,
	.size = 4,
	.align = 4,
},
builtin_type_u64 = {
	.storage = STORAGE_U64,
	.size = 8,
	.align = 8,
},
builtin_type_uint = {
	.storage = STORAGE_UINT,
	.size = 4,
	.align = 4,
},
builtin_type_uintptr = {
	.storage = STORAGE_UINTPTR,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_null = {
	.storage = STORAGE_NULL,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_rune = {
	.storage = STORAGE_RUNE,
	.size = 4,
	.align = 4,
},
builtin_type_size = {
	.storage = STORAGE_SIZE,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_void = {
	.storage = STORAGE_VOID,
	.size = 0,
	.align = 0,
},
builtin_type_const_bool = {
	.storage = STORAGE_BOOL,
	.flags = TYPE_CONST,
	.size = 4, // XXX: ARCH
	.align = 4,
},
builtin_type_const_char = {
	.storage = STORAGE_CHAR,
	.flags = TYPE_CONST,
	.size = 1,
	.align = 1,
},
builtin_type_const_f32 = {
	.storage = STORAGE_F32,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_f64 = {
	.storage = STORAGE_F64,
	.flags = TYPE_CONST,
	.size = 8,
	.align = 8,
},
builtin_type_const_fconst = {
	.storage = STORAGE_FCONST,
	.flags = TYPE_CONST,
	.size = SIZE_UNDEFINED,
	.align = ALIGN_UNDEFINED,
},
builtin_type_const_i8 = {
	.storage = STORAGE_I8,
	.flags = TYPE_CONST,
	.size = 1,
	.align = 1,
},
builtin_type_const_i16 = {
	.storage = STORAGE_I16,
	.flags = TYPE_CONST,
	.size = 2,
	.align = 2,
},
builtin_type_const_i32 = {
	.storage = STORAGE_I32,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_i64 = {
	.storage = STORAGE_I64,
	.flags = TYPE_CONST,
	.size = 8,
	.align = 8,
},
builtin_type_const_iconst = {
	.storage = STORAGE_ICONST,
	.flags = TYPE_CONST,
	.size = SIZE_UNDEFINED,
	.align = ALIGN_UNDEFINED,
},
builtin_type_const_int = {
	.storage = STORAGE_INT,
	.flags = TYPE_CONST,
	.size = 4, // XXX: ARCH
	.align = 4,
},
builtin_type_const_u8 = {
	.storage = STORAGE_U8,
	.flags = TYPE_CONST,
	.size = 1,
	.align = 1,
},
builtin_type_const_u16 = {
	.storage = STORAGE_U16,
	.flags = TYPE_CONST,
	.size = 2,
	.align = 2,
},
builtin_type_const_u32 = {
	.storage = STORAGE_U32,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_u64 = {
	.storage = STORAGE_U64,
	.flags = TYPE_CONST,
	.size = 8,
	.align = 8,
},
builtin_type_const_uint = {
	.storage = STORAGE_UINT,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_uintptr = {
	.storage = STORAGE_UINTPTR,
	.flags = TYPE_CONST,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_const_rune = {
	.storage = STORAGE_RUNE,
	.flags = TYPE_CONST,
	.size = 4,
	.align = 4,
},
builtin_type_const_size = {
	.storage = STORAGE_SIZE,
	.flags = TYPE_CONST,
	.size = 8, // XXX: ARCH
	.align = 8,
},
builtin_type_const_void = {
	.storage = STORAGE_VOID,
	.flags = TYPE_CONST,
	.size = 0,
	.align = 0,
};

// Others
struct type builtin_type_ptr_const_char = {
	.storage = STORAGE_POINTER,
	.size = 8, // XXX: ARCH
	.align = 8,
	.pointer = {
		.referent = &builtin_type_const_char,
	},
},
builtin_type_str = {
	.storage = STORAGE_STRING,
	.size = 24, // XXX: ARCH
	.align = 8,
},
builtin_type_const_str = {
	.storage = STORAGE_STRING,
	.flags = TYPE_CONST,
	.size = 24, // XXX: ARCH
	.align = 8,
};
