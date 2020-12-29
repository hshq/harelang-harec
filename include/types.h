#ifndef HARE_TYPES_H
#define HARE_TYPES_H
#include <stdbool.h>
#include <stdint.h>
#include "identifier.h"

enum type_storage {
	// Scalar types
	TYPE_STORAGE_BOOL,
	TYPE_STORAGE_CHAR,
	TYPE_STORAGE_ENUM,
	TYPE_STORAGE_F32,
	TYPE_STORAGE_F64,
	TYPE_STORAGE_I16,
	TYPE_STORAGE_I32,
	TYPE_STORAGE_I64,
	TYPE_STORAGE_I8,
	TYPE_STORAGE_INT,
	TYPE_STORAGE_NULL,
	TYPE_STORAGE_RUNE,
	TYPE_STORAGE_SIZE,
	TYPE_STORAGE_U16,
	TYPE_STORAGE_U32,
	TYPE_STORAGE_U64,
	TYPE_STORAGE_U8,
	TYPE_STORAGE_UINT,
	TYPE_STORAGE_UINTPTR,
	TYPE_STORAGE_VOID,
	// Aggregate types
	TYPE_STORAGE_ALIAS,
	TYPE_STORAGE_ARRAY,
	TYPE_STORAGE_FUNCTION,
	TYPE_STORAGE_POINTER,
	TYPE_STORAGE_SLICE,
	TYPE_STORAGE_STRING,
	TYPE_STORAGE_STRUCT,
	TYPE_STORAGE_TAGGED_UNION,
	TYPE_STORAGE_UNION,
};

struct type;

#define SIZE_UNDEFINED ((size_t)-1)
#define ALIGN_UNDEFINED ((size_t)-1)

struct type_array {
	size_t length; // SIZE_UNDEFINED for [*] or slices
	const struct type *members;
	bool expandable;
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

enum type_flags {
	TYPE_CONST = 1 << 0,
};

struct type {
	enum type_storage storage;
	unsigned int flags;
	size_t size, align;
	union {
		struct type_array array;
		struct type_func func;
		struct type_pointer pointer;
	};
};

const struct type *type_dereference(const struct type *type);

const char *type_storage_unparse(enum type_storage storage);
bool type_is_signed(const struct type *type);
bool type_is_integer(const struct type *type);
bool type_is_numeric(const struct type *type);
bool type_is_float(const struct type *type);

// Built-in type singletons
extern const struct type
	// Primitive
	builtin_type_bool,
	builtin_type_char,
	builtin_type_f32,
	builtin_type_f64,
	builtin_type_i8,
	builtin_type_i16,
	builtin_type_i32,
	builtin_type_i64,
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
	builtin_type_const_i8,
	builtin_type_const_i16,
	builtin_type_const_i32,
	builtin_type_const_i64,
	builtin_type_const_int,
	builtin_type_const_u8,
	builtin_type_const_u16,
	builtin_type_const_u32,
	builtin_type_const_u64,
	builtin_type_const_uint,
	builtin_type_const_uintptr,
	builtin_type_const_rune,
	builtin_type_const_size,
	// etc
	builtin_type_const_ptr_char,
	builtin_type_str,
	builtin_type_const_str;

#endif
