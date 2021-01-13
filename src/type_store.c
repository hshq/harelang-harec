#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "eval.h"
#include "scope.h"
#include "type_store.h"
#include "util.h"

static size_t
ast_array_len(struct type_store *store, const struct ast_type *atype)
{
	// TODO: Maybe we should cache these
	struct expression in, out;
	if (atype->array.length == NULL) {
		return SIZE_UNDEFINED;
	}
	check_expression(store->check_context, atype->array.length, &in);
	enum eval_result r = eval_expr(store->check_context, &in, &out);
	// TODO: Bubble up these errors:
	assert(r == EVAL_OK);
	assert(type_is_integer(out.result));
	if (type_is_signed(out.result)) {
		assert(out.constant.ival > 0);
	}
	return (size_t)out.constant.uval;
}

const struct type *
type_dealias(const struct type *type)
{
	while (type->storage == TYPE_STORAGE_ALIAS) {
		type = type->alias.type;
	}
	return type;
}

bool
type_is_assignable(struct type_store *store,
	const struct type *to,
	const struct type *from)
{
	// const and non-const types are mutually assignable
	if (to->flags & TYPE_CONST) {
		to = type_store_lookup_with_flags(store,
			to, to->flags & ~TYPE_CONST);
	}
	if (from->flags & TYPE_CONST) {
		from = type_store_lookup_with_flags(store,
			from, from->flags & ~TYPE_CONST);
	}

	if (to->storage == TYPE_STORAGE_ARRAY
			&& from->storage == TYPE_STORAGE_ARRAY) {
	}

	if (to == from) {
		return true;
	}

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
		switch (from->storage) {
		case TYPE_STORAGE_UINTPTR:
			return true;
		case TYPE_STORAGE_NULL:
			return to->pointer.flags & PTR_NULLABLE;
		case TYPE_STORAGE_POINTER:
			switch (to->pointer.referent->storage) {
			case TYPE_STORAGE_VOID:
				// TODO: const transitivity
				return to->pointer.referent->flags == from->pointer.referent->flags;
			case TYPE_STORAGE_ARRAY:
				if (type_is_assignable(store,
						to->pointer.referent,
						from->pointer.referent)) {
					return true;
				}
				break;
			default:
				if (to->pointer.referent != from->pointer.referent) {
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
				// TODO: const transitivity
				&& to->pointer.referent->flags & TYPE_CONST;
		default:
			return false;
		}
		assert(0); // Unreachable
	case TYPE_STORAGE_ALIAS:
		return type_is_assignable(store, to->alias.type, from);
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_TAGGED_UNION:
		assert(0); // TODO
	case TYPE_STORAGE_STRING:
		return to == &builtin_type_const_ptr_char;
	case TYPE_STORAGE_VOID:
		return true;
	case TYPE_STORAGE_SLICE:
		// XXX: This is not quite right
		to_secondary = type_store_lookup_with_flags(store,
			to->array.members, to->array.members->flags & ~TYPE_CONST);
		from_secondary = type_store_lookup_with_flags(store,
			from->array.members, from->array.members->flags & ~TYPE_CONST);
		return from->storage == TYPE_STORAGE_ARRAY
			&& to_secondary == from_secondary;
	case TYPE_STORAGE_ARRAY:
		return to->array.length == SIZE_UNDEFINED
			&& from->array.length != SIZE_UNDEFINED;
	// The following types are only assignable from themselves, and are
	// handled above:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		return false;
	}

	assert(0); // Unreachable
}

bool
type_is_castable(const struct type *to, const struct type *from)
{
	to = type_dealias(to);
	from = type_dealias(from);

	if (to == from) {
		return true;
	}

	switch (from->storage) {
	case TYPE_STORAGE_CHAR:
		return to->storage == TYPE_STORAGE_U8;
	case TYPE_STORAGE_ENUM:
		return to->storage == TYPE_STORAGE_ENUM || type_is_integer(to);
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		return type_is_numeric(to);
	case TYPE_STORAGE_U8:
		if (to->storage == TYPE_STORAGE_CHAR) {
			return true;
		}
		// Fallthrough
	case TYPE_STORAGE_U32:
		if (to->storage == TYPE_STORAGE_RUNE
				&& from->storage == TYPE_STORAGE_U32) {
			return true;
		}
		// Fallthrough
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
		return to->storage == TYPE_STORAGE_ENUM || type_is_numeric(to);
	case TYPE_STORAGE_UINTPTR:
		return to->storage == TYPE_STORAGE_POINTER
			|| to->storage == TYPE_STORAGE_NULL
			|| type_is_numeric(to);
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_NULL:
		return to->storage == TYPE_STORAGE_POINTER
			|| to->storage == TYPE_STORAGE_NULL
			|| to->storage == TYPE_STORAGE_UINTPTR;
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_ARRAY:
		return to->storage == TYPE_STORAGE_SLICE
			|| to->storage == TYPE_STORAGE_ARRAY;
	case TYPE_STORAGE_TAGGED_UNION:
		assert(0); // TODO
	case TYPE_STORAGE_STRING:
		return to->pointer.referent->storage == TYPE_STORAGE_CHAR
				&& to->pointer.referent->flags & TYPE_CONST;
	case TYPE_STORAGE_RUNE:
		return to->storage == TYPE_STORAGE_U32;
	// Cannot be cast:
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		return false;
	case TYPE_STORAGE_ALIAS:
		assert(0); // Handled above
	}

	assert(0); // Unreachable
}

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
		return is_const ? &builtin_type_const_void : &builtin_type_void;
	case TYPE_STORAGE_NULL:
		return &builtin_type_null; // const null and null are the same type
	case TYPE_STORAGE_STRING:
		return is_const ? &builtin_type_const_str : &builtin_type_str;
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_ENUM:
		return NULL;
	}
	assert(0); // Unreachable
}

static unsigned long
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
			hash = djb2_s(hash, ident->name);
		}
		hash = djb2(hash, type_hash(store, type->alias.type));
		break;
	case TYPE_STORAGE_ARRAY:
		hash = djb2(hash, type_hash(store, type->array.members));
		hash = djb2(hash, type->array.length);
		break;
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
		hash = djb2(hash, type_hash(store, type->pointer.referent));
		break;
	case TYPE_STORAGE_SLICE:
		hash = djb2(hash, type_hash(store, type->array.members));
		break;
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		for (const struct struct_field *field = type->struct_union.fields;
				field; field = field->next) {
			hash = djb2_s(hash, field->name);
			hash = djb2(hash, type_hash(store, field->type));
			hash = djb2(hash, field->offset);
		}
		break;
	case TYPE_STORAGE_TAGGED_UNION:
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

static int
type_cmp(const struct type *a, const struct type *b)
{
	if (a == b) {
		return 0;
	}
	if (a->storage != b->storage) {
		return a->storage - b->storage;
	}

	int c;
	switch (a->storage) {
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
		return 0;
	case TYPE_STORAGE_ALIAS:
		return identifier_cmp(&a->alias.ident, &b->alias.ident);
	case TYPE_STORAGE_ARRAY:
		if (a->array.length != b->array.length) {
			return a->array.length - b->array.length;
		}
		if (a->array.expandable != b->array.expandable) {
			return a->array.expandable - b->array.expandable;
		}
		return type_cmp(a->array.members, b->array.members);
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	case TYPE_STORAGE_FUNCTION:
		if (a->func.variadism != b->func.variadism) {
			return a->func.variadism - b->func.variadism;
		}
		if (a->func.flags != b->func.flags) {
			return a->func.flags - b->func.flags;
		}
		if ((c = type_cmp(a->func.result, b->func.result)) != 0) {
			return c;
		}
		struct type_func_param *aparam = a->func.params;
		struct type_func_param *bparam = b->func.params;
		while (aparam && bparam) {
			if ((c = type_cmp(aparam->type, bparam->type)) != 0) {
				return c;
			}

			aparam = aparam->next;
			bparam = bparam->next;
		}
		return aparam ? 1 : bparam ? -1 : 0;
	case TYPE_STORAGE_POINTER:
		if (a->pointer.flags != b->pointer.flags) {
			return a->pointer.flags - b->pointer.flags;
		}
		return type_cmp(a->pointer.referent, b->pointer.referent);
	case TYPE_STORAGE_SLICE:
		return type_cmp(a->array.members, b->array.members);
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		for (const struct struct_field *afield = a->struct_union.fields,
				*bfield = b->struct_union.fields;
				afield && bfield;
				afield = afield->next, bfield = bfield->next) {
			if (!!afield->next != !!bfield->next) {
				return afield->next ? 1 : bfield->next ? -1 : 0;
			}
			if ((c = strcmp(afield->name, bfield->name)) != 0) {
				return c;
			}
			if ((c = type_cmp(afield->type, bfield->type)) != 0) {
				return c;
			}
			if (afield->offset != bfield->offset) {
				return afield->offset - bfield->offset;
			}
		}
		return 0;
	case TYPE_STORAGE_TAGGED_UNION:
		assert(0); // TODO
	}

	assert(0); // Unreachable
}

static void
struct_insert_field(struct type_store *store, struct struct_field **fields,
	enum type_storage storage, size_t *size, size_t *usize, size_t *align,
	const struct ast_struct_union_type *atype)
{
	assert(atype->member_type == MEMBER_TYPE_FIELD);
	while (*fields && strcmp((*fields)->name, atype->field.name) < 0) {
		fields = &(*fields)->next;
	}
	struct struct_field *field = *fields;
	// TODO: Bubble this error up
	assert(field == NULL || strcmp(field->name, atype->field.name) != 0);
	*fields = xcalloc(1, sizeof(struct struct_field));
	(*fields)->next = field;
	field = *fields;

	field->name = strdup(atype->field.name);
	field->type = type_store_lookup_atype(store, atype->field.type);
	*size += *size % field->type->align;
	field->offset = *size;
	if (storage == TYPE_STORAGE_STRUCT) {
		*size += field->type->size;
	} else {
		*usize = field->type->size > *usize ? field->type->size : *usize;
	}
	*align = field->type->align > *align ? field->type->align : *align;
}

static void
struct_init_from_atype(struct type_store *store, enum type_storage storage,
	size_t *size, size_t *align, struct struct_field **fields,
	const struct ast_struct_union_type *atype)
{
	// TODO: fields with size SIZE_UNDEFINED
	size_t usize = 0;
	assert(storage == TYPE_STORAGE_STRUCT || storage == TYPE_STORAGE_UNION);
	while (atype) {
		size_t sub = *size;
		switch (atype->member_type) {
		case MEMBER_TYPE_FIELD:
			struct_insert_field(store, fields, storage, size, &usize,
				align, atype);
			break;
		case MEMBER_TYPE_EMBEDDED:
			struct_init_from_atype(store, atype->embedded->storage,
				&sub, align, fields,
				&atype->embedded->struct_union);
			if (storage == TYPE_STORAGE_UNION) {
				usize = sub > usize ? sub : usize;
			} else {
				*size += sub;
			}
			break;
		case MEMBER_TYPE_ALIAS:
			assert(0); // TODO
		}
		atype = atype->next;
	}
	if (storage == TYPE_STORAGE_UNION) {
		*size = usize;
	}
}

static void
type_init_from_atype(struct type_store *store,
	struct type *type,
	const struct ast_type *atype)
{
	type->storage = atype->storage;
	type->flags = atype->flags;

	const struct scope_object *obj;
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
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
		assert(0); // Invariant
	case TYPE_STORAGE_ALIAS:
		obj = scope_lookup(store->check_context->scope, &atype->alias);
 		// TODO: Bubble this up:
		assert(obj && obj->otype == O_TYPE);
		identifier_dup(&type->alias.ident, &atype->alias);
		type->alias.type = obj->type;
		type->size = type->alias.type->size;
		type->align = type->alias.type->align;
		break;
	case TYPE_STORAGE_ARRAY:
		type->array.length = ast_array_len(store, atype);
		type->array.members = type_store_lookup_atype(
				store, atype->array.members);
		// TODO: Bubble this up:
		assert(type->array.members->size != SIZE_UNDEFINED);

		type->align = type->array.members->align;
		if (type->array.length == SIZE_UNDEFINED) {
			type->size = SIZE_UNDEFINED;
		} else {
			type->size = type->array.members->size * type->array.length;
		}
		break;
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	case TYPE_STORAGE_FUNCTION:
		type->size = SIZE_UNDEFINED;
		type->align = SIZE_UNDEFINED;
		type->func.result =
			type_store_lookup_atype(store, atype->func.result);
		type->func.variadism = atype->func.variadism;
		type->func.flags = atype->func.flags;
		struct type_func_param *param, **next = &type->func.params;
		for (struct ast_function_parameters *aparam = atype->func.params;
				aparam; aparam = aparam->next) {
			param = *next = xcalloc(1, sizeof(struct type_func_param));
			param->type = type_store_lookup_atype(store, aparam->type);
			if (atype->func.variadism == VARIADISM_HARE
					&& !aparam->next) {
				param->type = type_store_lookup_slice(
					store, param->type);
			}
			next = &param->next;
		}
		break;
	case TYPE_STORAGE_POINTER:
		type->size = 8; // XXX: ARCH
		type->align = 8;
		type->pointer.flags = atype->pointer.flags;
		type->pointer.referent = type_store_lookup_atype(
			store, atype->pointer.referent);
		break;
	case TYPE_STORAGE_SLICE:
		type->size = 24; // XXX: ARCH
		type->align = 8;
		type->array.members = type_store_lookup_atype(
			store, atype->array.members);
		type->array.length = SIZE_UNDEFINED;
		break;
	case TYPE_STORAGE_STRUCT:
		type->struct_union.c_compat = true;
		// Fallthrough
	case TYPE_STORAGE_UNION:
		struct_init_from_atype(store, type->storage, &type->size,
			&type->align, &type->struct_union.fields,
			&atype->struct_union);
		break;
	case TYPE_STORAGE_TAGGED_UNION:
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
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_STRING:
		assert(0); // Invariant
	case TYPE_STORAGE_ALIAS:
		new->size = old->size;
		new->align = old->align;
		identifier_dup(&new->alias.ident, &old->alias.ident);
		new->alias.type =
			type_store_lookup_type(store, old->alias.type);
		break;
	case TYPE_STORAGE_ARRAY:
		new->array.members =
			type_store_lookup_type(store, old->array.members);
		new->array.length = old->array.length;
		new->array.expandable = old->array.expandable;
		new->align = new->array.members->align;
		if (new->array.length == SIZE_UNDEFINED) {
			new->size = SIZE_UNDEFINED;
		} else {
			new->size = new->array.members->size * new->array.length;
		}
		break;
	case TYPE_STORAGE_ENUM:
		assert(0); // TODO
	case TYPE_STORAGE_FUNCTION:
		new->size = SIZE_UNDEFINED;
		new->align = SIZE_UNDEFINED;
		new->func.result =
			type_store_lookup_type(store, old->func.result);
		new->func.variadism = old->func.variadism;
		for (struct type_func_param *oparam = old->func.params,
				*param, **next = &new->func.params; oparam;
				oparam = oparam->next) {
			param = *next =
				xcalloc(1, sizeof(struct type_func_param));
			param->type =
				type_store_lookup_type(store, oparam->type);
			next = &param->next;
		}
		new->func.flags = old->func.flags;
		break;
	case TYPE_STORAGE_POINTER:
		new->size = 8; // XXX: ARCH
		new->align = 8;
		new->pointer.flags = old->pointer.flags;
		new->pointer.referent = type_store_lookup_type(
			store, old->pointer.referent);
		break;
	case TYPE_STORAGE_SLICE:
		new->size = 24; // XXX: ARCH
		new->align = 8;
		new->array.members = type_store_lookup_type(
			store, old->array.members);
		new->array.length = SIZE_UNDEFINED;
		break;
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:;
		struct struct_field **next = &new->struct_union.fields;
		for (const struct struct_field *ofield = old->struct_union.fields;
				ofield; ofield = ofield->next) {
			struct struct_field *field = *next =
				xcalloc(sizeof(struct struct_field), 1);
			next = &field->next;
			field->name = ofield->name;
			field->type =
				type_store_lookup_type(store, ofield->type);
			field->offset = ofield->offset;
		}
		new->struct_union.c_compat = old->struct_union.c_compat;
		new->size = old->size;
		new->align = old->align;
		break;
	case TYPE_STORAGE_TAGGED_UNION:
		assert(0); // TODO
	}
}

// Used internally for looking up modified forms of other types and for
// inserting types into the type store
static const struct type *
type_store_lookup_type(struct type_store *store, const struct type *type)
{
	const struct type *builtin = builtin_for_type(type);
	if (builtin) {
		return builtin;
	}

	unsigned long hash = type_hash(store, type);
	struct type_bucket **next = &store->buckets[hash % TYPE_STORE_BUCKETS],
		*bucket = NULL;

	while (*next) {
		bucket = *next;
		if (type_cmp(&bucket->type, type) == 0) {
			return &bucket->type;
		}
		next = &bucket->next;
	}

	bucket = *next = xcalloc(1, sizeof(struct type_bucket));
	// XXX: can we replace this with memcpy?
	type_init_from_type(store, &bucket->type, type);
	return &bucket->type;
}

const struct type *
type_store_lookup_atype(struct type_store *store, const struct ast_type *atype)
{
	const struct type *builtin = builtin_for_atype(atype);
	if (builtin) {
		return builtin;
	}

	struct type type = {0};
	type_init_from_atype(store, &type, atype);
	return type_store_lookup_type(store, &type);
}

const struct type *
type_store_lookup_with_flags(struct type_store *store,
	const struct type *type, unsigned int flags)
{
	if (type->flags == flags) {
		return type;
	}
	struct type new = *type;
	new.flags = flags;
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

const struct type *
type_store_lookup_array(struct type_store *store,
	const struct type *members, size_t len, bool expandable)
{
	struct type array = {
		.storage = TYPE_STORAGE_ARRAY,
		.array = {
			.members = members,
			.length = len,
			.expandable = expandable,
		},
	};
	return type_store_lookup_type(store, &array);
}

const struct type *
type_store_lookup_slice(struct type_store *store, const struct type *members)
{
	struct type slice = {
		.storage = TYPE_STORAGE_SLICE,
		.array = {
			.members = members,
			.length = SIZE_UNDEFINED,
		},
	};
	return type_store_lookup_type(store, &slice);
}
