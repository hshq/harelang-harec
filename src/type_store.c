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

static bool
tagged_assignable(struct type_store *store,
		const struct type *to,
		const struct type *from)
{
	if (from->storage == TYPE_STORAGE_TAGGED_UNION) {
		if (to->storage != TYPE_STORAGE_TAGGED_UNION) {
			return false;
		}
		// Only assignable if 'to' is a superset of 'from'
		// Invariant: type_tagged_union is sorted by type tag
		const struct type_tagged_union *to_t = &to->tagged;
		const struct type_tagged_union *from_t = &from->tagged;
		while (to_t && from_t) {
			if (to_t->type->id == from_t->type->id) {
				to_t = to_t->next;
				from_t = from_t->next;
			} else {
				from_t = from_t->next;
			}
		}
		return from_t == NULL;
	}

	size_t nassignable = 0;
	for (const struct type_tagged_union *tu = &to->tagged;
			tu; tu = tu->next) {
		if (tu->type->id == from->id) {
			return true;
		}
		if (type_is_assignable(store, tu->type, from)) {
			++nassignable;
		}
	}

	return nassignable == 1;
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
		return (from->storage == TYPE_STORAGE_ARRAY
				|| from->storage == TYPE_STORAGE_SLICE)
			&& to_secondary == from_secondary;
	case TYPE_STORAGE_ARRAY:
		return from->storage == TYPE_STORAGE_ARRAY
			&& to->array.length == SIZE_UNDEFINED
			&& from->array.length != SIZE_UNDEFINED;
	case TYPE_STORAGE_TAGGED_UNION:
		return tagged_assignable(store, to, from);
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

static bool
tagged_castable(const struct type *to, const struct type *from)
{
	if (to->storage == TYPE_STORAGE_TAGGED_UNION) {
		if (from->storage == TYPE_STORAGE_TAGGED_UNION) {
			return true;
		}
		assert(0); // TODO
	}

	// TODO: Update spec to make this consistent
	size_t ncastable = 0;
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
		return tagged_castable(to, from);
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

static size_t
sum_atagged_memb(const struct ast_tagged_union_type *u)
{
	size_t nmemb = 0;
	for (; u; u = u->next) {
		if (u->type->storage == TYPE_STORAGE_TAGGED_UNION) {
			nmemb += sum_atagged_memb(&u->type->tagged_union);
		} else {
			++nmemb;
		}
	}
	return nmemb;
}

static void
collect_atagged_memb(struct type_store *store,
		struct type_tagged_union **ta,
		const struct ast_tagged_union_type *atu,
		size_t *i)
{
	for (; atu; atu = atu->next) {
		if (atu->type->storage == TYPE_STORAGE_TAGGED_UNION) {
			collect_atagged_memb(store, ta, &atu->type->tagged_union, i);
			continue;
		}
		struct type_tagged_union *tu;
		ta[*i] = tu = xcalloc(1, sizeof(struct type_tagged_union));
		tu->type = type_store_lookup_atype(store, atu->type);
		*i += 1;
	}
}

static int
tagged_cmp(const void *ptr_a, const void *ptr_b)
{
	const struct type_tagged_union **a =
		(const struct type_tagged_union **)ptr_a;
	const struct type_tagged_union **b =
		(const struct type_tagged_union **)ptr_b;
	return (*a)->type->id < (*b)->type->id ? -1
		: (*a)->type->id > (*b)->type->id ? 1 : 0;
}

static void
tagged_init_from_atype(struct type_store *store,
	struct type *type, const struct ast_type *atype)
{
	size_t nmemb = sum_atagged_memb(&atype->tagged_union);
	struct type_tagged_union **tu =
		xcalloc(nmemb, sizeof(struct type_tagged_union *));
	size_t i = 0;
	collect_atagged_memb(store, tu, &atype->tagged_union, &i);

	// Prune duplicates
	for (i = 1; i < nmemb; ++i)
	for (size_t j = 0; j < i; ++j) {
		if (tu[j]->type->id == tu[i]->type->id) {
			assert(0); // TODO: prune
		}
	}

	// Sort by ID
	qsort(tu, nmemb, sizeof(tu[0]), tagged_cmp);

	// First one free
	type->tagged = *tu[0];
	free(tu[0]);

	type->size = type->tagged.type->size;
	type->align = type->tagged.type->align;

	struct type_tagged_union **next = &type->tagged.next;
	for (size_t i = 1; i < nmemb; ++i) {
		if (tu[i]->type->size > type->size) {
			type->size = tu[i]->type->size;
		}
		if (tu[i]->type->align > type->align) {
			type->align = tu[i]->type->align;
		}

		*next = tu[i];
		next = &tu[i]->next;
	}

	type->size += builtin_type_size.size;
	if (builtin_type_size.align > type->align) {
		type->align = builtin_type_size.align;
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
		if (atype->unwrap) {
			*type = *obj->type;
			break;
		}
		assert(obj && obj->otype == O_TYPE); // TODO: Bubble this up
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
		tagged_init_from_atype(store, type, atype);
		break;
	}
}

static const struct type *
type_store_lookup_type(struct type_store *store, const struct type *type)
{
	const struct type *builtin = builtin_for_type(type);
	if (builtin) {
		return builtin;
	}

	uint64_t hash = type_hash(type);
	struct type_bucket **next = &store->buckets[hash % TYPE_STORE_BUCKETS],
		*bucket = NULL;

	while (*next) {
		bucket = *next;
		if (bucket->type.id == hash) {
			return &bucket->type;
		}
		next = &bucket->next;
	}

	bucket = *next = xcalloc(1, sizeof(struct type_bucket));
	bucket->type = *type;
	bucket->type.id = hash;
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
		.size = 8, // XXX: ARCH
		.align = 8,
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
		.size = len == SIZE_UNDEFINED
			? SIZE_UNDEFINED : members->size * len,
		.align = members->align,
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
		.size = 24, // XXX: ARCH
		.align = 8,
	};
	return type_store_lookup_type(store, &slice);
}
