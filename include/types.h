#ifndef HARE_TYPES_H
#define HARE_TYPES_H
#include <stdbool.h>
#include <stdint.h>
#include "identifier.h"

enum type_storage {
	// Built-in types
	// The order of these is important
	STORAGE_BOOL,
	STORAGE_CHAR,
	STORAGE_ENUM,
	STORAGE_F32,
	STORAGE_F64,
	STORAGE_I16,
	STORAGE_I32,
	STORAGE_I64,
	STORAGE_I8,
	STORAGE_INT,
	STORAGE_NULL,
	STORAGE_RUNE,
	STORAGE_SIZE,
	STORAGE_STRING,
	STORAGE_U16,
	STORAGE_U32,
	STORAGE_U64,
	STORAGE_U8,
	STORAGE_UINT,
	STORAGE_UINTPTR,
	STORAGE_VOID,
	// Other types
	STORAGE_ALIAS,
	STORAGE_ARRAY,
	STORAGE_FUNCTION,
	STORAGE_POINTER,
	STORAGE_SLICE,
	STORAGE_STRUCT,
	STORAGE_TAGGED,
	STORAGE_TUPLE,
	STORAGE_UNION,
	STORAGE_TYPE,
	STORAGE_FCONST,
	STORAGE_ICONST,
};

struct type;

#define SIZE_UNDEFINED ((size_t)-1)
#define ALIGN_UNDEFINED ((size_t)-1)

struct type_alias {
	struct identifier ident;
	const struct type *type;
	bool exported; // Used to make sure unexported aliases aren't emitted
};

struct type_array {
	size_t length; // SIZE_UNDEFINED for [*] and slices
	const struct type *members;
};

struct type_enum_value {
	char *name;
	struct type_enum_value *next;
	union {
		int64_t ival;
		uint64_t uval;
	};
};

struct type_enum {
	enum type_storage storage;
	struct type_enum_value *values;
};

enum variadism {
	VARIADISM_NONE,
	VARIADISM_C,
	VARIADISM_HARE,
};

enum function_flags {
	FN_NORETURN = 1 << 0,
};

struct type_func_param {
	const struct type *type;
	struct type_func_param *next;
};

struct type_func {
	const struct type *result;
	enum variadism variadism;
	struct type_func_param *params;
	unsigned int flags; // enum function_flags
};

enum pointer_flags {
	PTR_NULLABLE = 1 << 0,
};

struct type_pointer {
	const struct type *referent;
	unsigned int flags;
};

struct struct_field {
	char *name;
	const struct type *type;
	size_t offset;
	struct struct_field *next;
};

struct type_struct_union {
	// c_compat is false if explicit offsets are used, or if the type is a
	// union. The latter is actually still C-compatible, but this is an
	// implementation detail which changes the way the QBE IL is generated,
	// and in the case of unions, the altered behavior for explicit-offset
	// structs is also correct for all cases of unions.
	bool c_compat;
	struct struct_field *fields;
};

struct type_tuple {
	const struct type *type;
	size_t offset;
	struct type_tuple *next;
};

struct type_tagged_union {
	const struct type *type;
	struct type_tagged_union *next;
};

enum type_flags {
	TYPE_CONST = 1 << 0,
	TYPE_ERROR = 1 << 1,
};

struct type {
	enum type_storage storage;
	uint32_t id;
	unsigned int flags;
	size_t size, align;
	union {
		struct type_alias alias;
		struct type_array array;
		struct type_enum _enum;
		struct type_func func;
		struct type_pointer pointer;
		struct type_struct_union struct_union;
		struct type_tagged_union tagged;
		struct type_tuple tuple;
	};
};

const struct type *type_dereference(const struct type *type);
const struct type *type_dealias(const struct type *type);
const struct struct_field *type_get_field(
	const struct type *type, const char *name);
const struct type_tuple *type_get_value(
	const struct type *type, uintmax_t index);

const struct type *tagged_select_subtype(
	const struct type *tagged, const struct type *subtype);
bool tagged_subset_compat(const struct type *to, const struct type *from);

const char *type_storage_unparse(enum type_storage storage);
bool storage_is_flexible(enum type_storage storage);
bool type_is_signed(const struct type *type);
bool type_is_integer(const struct type *type);
bool type_is_numeric(const struct type *type);
bool type_is_float(const struct type *type);
bool type_has_error(const struct type *type);

uint32_t type_hash(const struct type *type);

bool type_is_assignable(const struct type *to, const struct type *from);
bool type_is_castable(const struct type *to, const struct type *from);

void builtin_types_init();

// Built-in type singletons
extern struct type
	// Primitive
	builtin_type_bool,
	builtin_type_char,
	builtin_type_f32,
	builtin_type_f64,
	builtin_type_fconst,
	builtin_type_i8,
	builtin_type_i16,
	builtin_type_i32,
	builtin_type_i64,
	builtin_type_iconst,
	builtin_type_int,
	builtin_type_u8,
	builtin_type_u16,
	builtin_type_u32,
	builtin_type_u64,
	builtin_type_uint,
	builtin_type_uintptr,
	builtin_type_null,
	builtin_type_rune,
	builtin_type_size,
	builtin_type_void,
	// Const primitives
	builtin_type_const_bool,
	builtin_type_const_char,
	builtin_type_const_f32,
	builtin_type_const_f64,
	builtin_type_const_fconst,
	builtin_type_const_i8,
	builtin_type_const_i16,
	builtin_type_const_i32,
	builtin_type_const_i64,
	builtin_type_const_iconst,
	builtin_type_const_int,
	builtin_type_const_u8,
	builtin_type_const_u16,
	builtin_type_const_u32,
	builtin_type_const_u64,
	builtin_type_const_uint,
	builtin_type_const_uintptr,
	builtin_type_const_rune,
	builtin_type_const_size,
	builtin_type_const_void,
	// etc
	builtin_type_ptr_const_char,
	builtin_type_str,
	builtin_type_const_str;

#endif
