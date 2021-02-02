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
	check_expression(store->check_context, atype->array.length, &in, NULL);
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
	case TYPE_STORAGE_TAGGED:
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
	while (fields && *fields && strcmp((*fields)->name, atype->field.name) < 0) {
		fields = &(*fields)->next;
	}
	struct struct_field *field, _temp = {0};
	if (fields != NULL) {
		field = *fields;
		assert(field == NULL || strcmp(field->name, atype->field.name) != 0);
		*fields = xcalloc(1, sizeof(struct struct_field));
		(*fields)->next = field;
		field = *fields;
	} else {
		field = &_temp;
	}
	// TODO: Bubble this error up

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
			struct_insert_field(store, fields, storage,
				size, &usize, align, atype);
			break;
		case MEMBER_TYPE_EMBEDDED:
			if (atype->embedded->storage == TYPE_STORAGE_UNION) {
				// We need to set the offset of all union
				// members to the maximum alignment of the union
				// members, so first we do a dry run to compute
				// it:
				size_t offs = 0, align_1 = 0;
				struct_init_from_atype(store, TYPE_STORAGE_UNION,
					&offs, &align_1, NULL,
					&atype->embedded->struct_union);
				// Insert padding per the results:
				*size += *size % align_1;
				// Then insert the fields for real:
				sub = *size;
				struct_init_from_atype(store, TYPE_STORAGE_UNION,
					&sub, align, fields,
					&atype->embedded->struct_union);
			} else {
				struct_init_from_atype(store, TYPE_STORAGE_STRUCT,
					&sub, align, fields,
					&atype->embedded->struct_union);
			}

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
sum_tagged_memb(struct type_store *store,
		const struct type_tagged_union *u)
{
	size_t nmemb = 0;
	for (; u; u = u->next) {
		const struct type *type = u->type;
		if (type->storage == TYPE_STORAGE_TAGGED) {
			nmemb += sum_tagged_memb(store, &type->tagged);
		} else {
			++nmemb;
		}
	}
	return nmemb;
}

static size_t
sum_atagged_memb(struct type_store *store,
		const struct ast_tagged_union_type *u)
{
	size_t nmemb = 0;
	for (; u; u = u->next) {
		const struct type *type =
			type_store_lookup_atype(store, u->type);
		if (type->storage == TYPE_STORAGE_TAGGED) {
			nmemb += sum_tagged_memb(store, &type->tagged);
		} else {
			++nmemb;
		}
	}
	return nmemb;
}

static void
collect_tagged_memb(struct type_store *store,
		struct type_tagged_union **ta,
		const struct type_tagged_union *src,
		size_t *i)
{
	for (; src; src = src->next) {
		const struct type *type = src->type;
		if (type->storage == TYPE_STORAGE_TAGGED) {
			collect_tagged_memb(store, ta, &type->tagged, i);
			continue;
		}
		struct type_tagged_union *tu;
		ta[*i] = tu = xcalloc(1, sizeof(struct type_tagged_union));
		tu->type = type;
		*i += 1;
	}
}

static void
collect_atagged_memb(struct type_store *store,
		struct type_tagged_union **ta,
		const struct ast_tagged_union_type *atu,
		size_t *i)
{
	for (; atu; atu = atu->next) {
		const struct type *type =
			type_store_lookup_atype(store, atu->type);
		if (type->storage == TYPE_STORAGE_TAGGED) {
			collect_tagged_memb(store, ta, &type->tagged, i);
			continue;
		}
		struct type_tagged_union *tu;
		ta[*i] = tu = xcalloc(1, sizeof(struct type_tagged_union));
		tu->type = type;
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
tagged_init(struct type *type, struct type_tagged_union **tu, size_t nmemb)
{
	// Prune duplicates
	for (size_t i = 1; i < nmemb; ++i)
	for (size_t j = 0; j < i; ++j) {
		if (tu[j]->type->id == tu[i]->type->id) {
			memmove(&tu[i], &tu[i + 1], nmemb - i - 1);
			--nmemb;
			break;
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
		assert(tu[i]->type->size != SIZE_UNDEFINED); // TODO
		if (tu[i]->type->size > type->size) {
			type->size = tu[i]->type->size;
		}
		if (tu[i]->type->align > type->align) {
			type->align = tu[i]->type->align;
		}

		*next = tu[i];
		next = &tu[i]->next;
	}

	if (type->align == 0) {
		type->align = builtin_type_uint.align;
	}

	type->size += builtin_type_uint.size % type->align
		+ builtin_type_uint.align;
	if (type->align < builtin_type_uint.align) {
		type->align = builtin_type_uint.align;
	}
}

static void
tagged_init_from_atype(struct type_store *store,
	struct type *type, const struct ast_type *atype)
{
	size_t nmemb = sum_atagged_memb(store, &atype->tagged_union);
	struct type_tagged_union **tu =
		xcalloc(nmemb, sizeof(struct type_tagged_union *));
	size_t i = 0;
	collect_atagged_memb(store, tu, &atype->tagged_union, &i);
	tagged_init(type, tu, nmemb);
}

static const struct type *type_store_lookup_type(struct type_store *store, const struct type *type);

static void
type_init_from_atype(struct type_store *store,
	struct type *type,
	const struct ast_type *atype)
{
	type->storage = atype->storage;
	type->flags = atype->flags;

	const struct scope_object *obj;
	const struct identifier *ident;
	struct identifier temp;
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
		ident = &atype->alias;
		if (ident->ns == NULL) {
			temp = *ident;
			temp.ns = store->check_context->ns;
			ident = &temp;
		}

		obj = scope_lookup(store->check_context->scope, ident);
		if (!obj) {
			assert(!atype->unwrap);
			identifier_dup(&type->alias.ident, ident);
			type->alias.type = NULL;
			type->size = SIZE_UNDEFINED;
			type->align = SIZE_UNDEFINED;
			return;
		}

		assert(obj->otype == O_TYPE); // TODO: Bubble this up
		if (atype->unwrap) {
			*type = *type_dealias(obj->type);
			break;
		}

		identifier_dup(&type->alias.ident, &obj->ident);
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
		type->_enum.storage = atype->_enum.storage;
		const struct type *storage =
			builtin_type_for_storage(type->_enum.storage, true);
 		// TODO: Bubble this up
		assert(type_is_integer(storage));
		type->size = storage->size;
		type->align = storage->size;

		// TODO: Check for duplicates
		struct ast_enum_field *avalue = atype->_enum.values;
		struct type_enum_value **values = &type->_enum.values;
		intmax_t iimplicit = 0;
		uintmax_t uimplicit = 0;
		while (avalue) {
			struct type_enum_value *value = *values =
				xcalloc(sizeof(struct type_enum_value), 1);
			value->name = strdup(avalue->name);
			if (avalue->value != NULL) {
				struct expression in, out;
				check_expression(store->check_context,
					avalue->value, &in, NULL);
				enum eval_result r =
					eval_expr(store->check_context, &in, &out);
				// TODO: Bubble this up
				assert(r == EVAL_OK && type_is_assignable(storage, out.result));
				if (type_is_signed(storage)) {
					iimplicit = out.constant.ival;
				} else {
					uimplicit = out.constant.uval;
				}
			}
			// TODO: Test that the value fits into this precision
			if (type_is_signed(storage)) {
				value->ival = iimplicit++;
			} else {
				value->uval = uimplicit++;
			}
			values = &value->next;
			avalue = avalue->next;
		}
		break;
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
	case TYPE_STORAGE_TAGGED:
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

	uint32_t hash = type_hash(type);
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
	const struct type *members, size_t len)
{
	struct type array = {
		.storage = TYPE_STORAGE_ARRAY,
		.array = {
			.members = members,
			.length = len,
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

const struct type *
type_store_lookup_alias(struct type_store *store,
	const struct identifier *ident, const struct type *secondary)
{
	struct type alias = {
		.storage = TYPE_STORAGE_ALIAS,
		.alias = {
			.ident = *ident,
			.type = secondary,
		},
		.size = secondary->size,
		.align = secondary->align,
	};
	struct type *type = (struct type *)type_store_lookup_type(store, &alias);
	if (type->alias.type == NULL) {
		// Finish filling in forward referenced type
		type->alias.type = secondary;
		type->size = secondary->size;
		type->align = secondary->align;
	}
	return type;
}

const struct type *
type_store_lookup_tagged(struct type_store *store,
		struct type_tagged_union *tags)
{
	struct type type = {
		.storage = TYPE_STORAGE_TAGGED,
	};
	size_t nmemb = sum_tagged_memb(store, tags);
	struct type_tagged_union **tu =
		xcalloc(nmemb, sizeof(struct type_tagged_union *));
	size_t i = 0;
	collect_tagged_memb(store, tu, tags, &i);
	tagged_init(&type, tu, nmemb);
	return type_store_lookup_type(store, &type);
}
