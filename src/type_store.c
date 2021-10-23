#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "check.h"
#include "eval.h"
#include "scope.h"
#include "typedef.h"
#include "type_store.h"
#include "util.h"

static void
error(struct context *ctx, const struct location loc, char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	size_t sz = vsnprintf(NULL, 0, fmt, ap);
	va_end(ap);
	char *msg = xcalloc(1, sz + 1);
	va_start(ap, fmt);
	vsnprintf(msg, sz + 1, fmt, ap);
	va_end(ap);

	struct errors *next = *ctx->next = xcalloc(1, sizeof(struct errors));
	next->loc = loc;
	next->msg = msg;
	ctx->next = &next->next;
}

static char *
gen_typename(const struct type *type)
{
	size_t sz = 0;
	char *ptr = NULL;
	FILE *f = open_memstream(&ptr, &sz);
	emit_type(type, f);
	fclose(f);
	return ptr;
}

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
	if (r != EVAL_OK) {
		error(store->check_context, atype->loc,
			"Cannot evaluate array length at compile time");
		return SIZE_UNDEFINED;
	}
	if (!type_is_integer(out.result)) {
		error(store->check_context, atype->loc,
			"Array length must be an integer");
		return SIZE_UNDEFINED;
	}
	if (type_is_signed(out.result) && out.constant.ival <= 0) {
		error(store->check_context, atype->loc,
			"Array length must be greater than 0");
		return SIZE_UNDEFINED;
	}
	return (size_t)out.constant.uval;
}

const struct type *
builtin_type_for_storage(enum type_storage storage, bool is_const)
{
	switch (storage) {
	case STORAGE_BOOL:
		return is_const ? &builtin_type_const_bool : &builtin_type_bool;
	case STORAGE_CHAR:
		return is_const ? &builtin_type_const_char : &builtin_type_char;
	case STORAGE_F32:
		return is_const ? &builtin_type_const_f32 : &builtin_type_f32;
	case STORAGE_F64:
		return is_const ? &builtin_type_const_f64 : &builtin_type_f64;
	case STORAGE_FCONST:
		return is_const ? &builtin_type_const_fconst : &builtin_type_fconst;
	case STORAGE_I8:
		return is_const ? &builtin_type_const_i8 : &builtin_type_i8;
	case STORAGE_I16:
		return is_const ? &builtin_type_const_i16 : &builtin_type_i16;
	case STORAGE_I32:
		return is_const ? &builtin_type_const_i32 : &builtin_type_i32;
	case STORAGE_I64:
		return is_const ? &builtin_type_const_i64 : &builtin_type_i64;
	case STORAGE_ICONST:
		return is_const ? &builtin_type_const_iconst : &builtin_type_iconst;
	case STORAGE_INT:
		return is_const ? &builtin_type_const_int : &builtin_type_int;
	case STORAGE_RUNE:
		return is_const ? &builtin_type_const_rune : &builtin_type_rune;
	case STORAGE_SIZE:
		return is_const ? &builtin_type_const_size : &builtin_type_size;
	case STORAGE_U8:
		return is_const ? &builtin_type_const_u8 : &builtin_type_u8;
	case STORAGE_U16:
		return is_const ? &builtin_type_const_u16 : &builtin_type_u16;
	case STORAGE_U32:
		return is_const ? &builtin_type_const_u32 : &builtin_type_u32;
	case STORAGE_U64:
		return is_const ? &builtin_type_const_u64 : &builtin_type_u64;
	case STORAGE_UINT:
		return is_const ? &builtin_type_const_uint : &builtin_type_uint;
	case STORAGE_UINTPTR:
		return is_const ? &builtin_type_const_uintptr : &builtin_type_uintptr;
	case STORAGE_VOID:
		return is_const ? &builtin_type_const_void : &builtin_type_void;
	case STORAGE_NULL:
		return &builtin_type_null; // const null and null are the same type
	case STORAGE_STRING:
		return is_const ? &builtin_type_const_str : &builtin_type_str;
	case STORAGE_TYPE:
	case STORAGE_ALIAS:
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_ENUM:
		return NULL;
	}
	assert(0); // Unreachable
}

static const struct type *
builtin_for_atype(const struct ast_type *atype)
{
	if (atype->flags & TYPE_ERROR) {
		return NULL;
	}
	bool is_const = (atype->flags & TYPE_CONST) != 0;
	return builtin_type_for_storage(atype->storage, is_const);
}

static const struct type *
builtin_for_type(const struct type *type)
{
	if (type->flags & TYPE_ERROR) {
		return NULL;
	}
	bool is_const = (type->flags & TYPE_CONST) != 0;
	return builtin_type_for_storage(type->storage, is_const);
}

static struct struct_field *
struct_insert_field(struct type_store *store, struct struct_field **fields,
	enum type_storage storage, size_t *size, size_t *usize, size_t *align,
	const struct ast_struct_union_type *atype, bool *ccompat)
{
	while (*fields && (!atype->name || !(*fields)->name || strcmp((*fields)->name, atype->name) < 0)) {
		fields = &(*fields)->next;
	}
	struct struct_field *field = *fields;
	if (field != NULL && atype->name && field->name && strcmp(field->name, atype->name) == 0) {
		error(store->check_context, atype->type->loc,
			"Duplicate struct/union member '%s'", atype->name);
		return NULL;
	}
	*fields = xcalloc(1, sizeof(struct struct_field));
	(*fields)->next = field;
	field = *fields;

	if (atype->name) {
		field->name = strdup(atype->name);
	}
	field->type = type_store_lookup_atype(store, atype->type);
	if (field->type->size == 0) {
		error(store->check_context, atype->type->loc,
			"Struct field size cannot be zero");
		return NULL;
	}

	if (atype->offset) {
		*ccompat = false;
		struct expression in, out;
		check_expression(store->check_context, atype->offset, &in, NULL);
		enum eval_result r = eval_expr(store->check_context, &in, &out);
		if (r != EVAL_OK) {
			error(store->check_context, in.loc,
				"Cannot evaluate field offset at compile time");
		} else if (!type_is_integer(out.result)) {
			error(store->check_context, in.loc,
				"Field offset must be an integer");
		} else if (type_is_signed(out.result) && out.constant.ival < 0) {
			error(store->check_context, in.loc,
				"Field offset must not be less than 0");
		} else {
			size_t offs = (size_t)out.constant.uval;
			field->offset = offs;
		}
	} else {
		size_t offs = *size;
		if (offs % field->type->align) {
			offs += field->type->align - (offs % field->type->align);
		}
		field->offset = offs;
		assert(field->offset % field->type->align == 0);
	}

	if (field->type->size == SIZE_UNDEFINED || *size == SIZE_UNDEFINED) {
		*size = SIZE_UNDEFINED;
	} else if (storage == STORAGE_STRUCT) {
		*size = field->offset + field->type->size;
	} else {
		*usize = field->type->size > *usize ? field->type->size : *usize;
	}
	*align = field->type->align > *align ? field->type->align : *align;
	return field;
}

static const struct type *type_store_lookup_type(struct type_store *store, const struct type *type);

void
shift_fields(struct type_store *store, struct struct_field *parent)
{
	if (parent->type->storage == STORAGE_ALIAS
			&& type_dealias(parent->type)->storage != STORAGE_STRUCT
			&& type_dealias(parent->type)->storage != STORAGE_UNION) {
		// TODO
		struct location loc = {
			.path = "<unknown>",
			.lineno = 0,
			.colno = 0,
		};
		error(store->check_context, loc,
			"Cannot embed non-struct non-union alias");
		return;
	}
	if (parent->offset == 0) {
		// We need to return early here in order to avoid dealiasing an
		// embedded alias. This is acceptable at nonzero offsets, but we
		// need to keep the alias if it's at offset 0 because of
		// subtyping.
		return;
	}
	const struct type *type = type_dealias(parent->type);
	assert(type->storage == STORAGE_STRUCT
		|| type->storage == STORAGE_UNION);
	struct type new = {
		.storage = type->storage,
		.flags = type->flags,
		.size = type->size,
		.align = type->align,
		.struct_union.c_compat = type->struct_union.c_compat,
	};
	struct struct_field **next = &new.struct_union.fields;
	for (struct struct_field *field = type->struct_union.fields; field;
			field = field->next) {
		struct struct_field *new = *next =
			xcalloc(1, sizeof(struct struct_field));
		next = &new->next;
		new->type = field->type;
		new->offset = parent->offset;
		if (field->name) {
			new->name = strdup(field->name);
		} else {
			shift_fields(store, new);
		}
		// Sub-subfields are shifted by field->offset in the recursive
		// shift_fields call, delay adding it to new->offset to avoid
		// shifting by field->offset twice
		new->offset += field->offset;
	}

	parent->type = type_store_lookup_type(store, &new);
}

static void
struct_init_from_atype(struct type_store *store, enum type_storage storage,
	size_t *size, size_t *align, struct struct_field **fields,
	const struct ast_struct_union_type *atype, bool *ccompat)
{
	// TODO: fields with size SIZE_UNDEFINED
	size_t usize = 0;
	assert(storage == STORAGE_STRUCT || storage == STORAGE_UNION);
	while (atype) {
		struct struct_field *field = struct_insert_field(store, fields,
			storage, size, &usize, align, atype, ccompat);
		if (!field->name) {
			// We need to shift the embedded struct/union's fields
			// so that their offsets are from the start of the
			// parent type. This is a bit of a hack, but it makes
			// type_get_field far easier to implement and doesn't
			// cause any trouble in gen since offsets are only used
			// there for sorting fields.
			shift_fields(store, field);
		}
		atype = atype->next;
	}

	if (storage == STORAGE_UNION) {
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
		if (type->storage == STORAGE_TAGGED) {
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
		if (type->storage == STORAGE_TAGGED) {
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
		if (type->storage == STORAGE_TAGGED) {
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
		if (type->storage == STORAGE_TAGGED) {
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

static size_t
tagged_init(struct type_store *store, struct type *type,
		struct type_tagged_union **tu, size_t nmemb)
{
	// Sort by ID
	qsort(tu, nmemb, sizeof(tu[0]), tagged_cmp);

	// Prune duplicates
	size_t nmemb_dedup = 1;
	for (size_t i = 1; i < nmemb; ++i) {
		if (tu[i]->type->id != tu[nmemb_dedup - 1]->type->id) {
			tu[nmemb_dedup++] = tu[i];
		}
	}
	nmemb = nmemb_dedup;

	if (tu[0]->type->size == SIZE_UNDEFINED) {
		char *type = gen_typename(tu[0]->type);
		// TODO
		struct location loc = {
			.path = "<unknown>",
			.lineno = 0,
			.colno = 0,
		};
		error(store->check_context, loc,
			"Cannot create tagged union from type %s, which has undefined size\n",
			type);
		free(type);
	}

	// First one free
	type->tagged = *tu[0];
	free(tu[0]);

	type->size = type->tagged.type->size;
	type->align = type->tagged.type->align;

	struct type_tagged_union **next = &type->tagged.next;
	for (size_t i = 1; i < nmemb; ++i) {
		if (tu[i]->type->size == SIZE_UNDEFINED) {
			char *type = gen_typename(tu[i]->type);
			// TODO
			struct location loc = {
				.path = "<unknown>",
				.lineno = 0,
				.colno = 0,
			};
			error(store->check_context, loc,
				"Cannot create tagged union from type %s, which has undefined size\n",
				type);
			free(type);
		}

		if (tu[i]->type->size > type->size) {
			type->size = tu[i]->type->size;
		}
		if (tu[i]->type->align > type->align) {
			type->align = tu[i]->type->align;
		}

		*next = tu[i];
		next = &tu[i]->next;
	}

	if (type->align < builtin_type_uint.align) {
		type->align = builtin_type_uint.align;
	}
	type->size += builtin_type_uint.size % type->align
		+ builtin_type_uint.align;

	return nmemb;
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
	nmemb = tagged_init(store, type, tu, nmemb);

	if (nmemb <= 1) {
		error(store->check_context, atype->loc,
			"Cannot create tagged union with a single member");
	}
}

static void
tuple_init_from_atype(struct type_store *store,
	struct type *type, const struct ast_type *atype)
{
	type->size = 0, type->align = 0;
	const struct ast_tuple_type *atuple = &atype->tuple;
	struct type_tuple *cur = &type->tuple;
	while (atuple) {
		cur->type = type_store_lookup_atype(store, atuple->type);
		if (cur->type->size == 0) {
			error(store->check_context, atuple->type->loc,
				"Type of size zero cannot be a tuple member");
			continue;
		}
		cur->offset = type->size % cur->type->align + type->size;
		type->size += type->size % cur->type->align + cur->type->size;
		if (type->align < cur->type->align) {
			type->align = cur->type->align;
		}

		atuple = atuple->next;
		if (atuple) {
			cur->next = xcalloc(1, sizeof(struct type_tuple));
			cur = cur->next;
		}
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
	const struct identifier *ident;
	const struct type *builtin;
	struct identifier temp;
	switch (type->storage) {
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		assert(0); // Invariant
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
	case STORAGE_NULL:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_STRING:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VOID:
		builtin = builtin_type_for_storage(type->storage, false);
		type->size = builtin->size;
		type->align = builtin->align;
		break;
	case STORAGE_ALIAS:
		ident = &atype->alias;
		if (ident->ns == NULL) {
			temp = *ident;
			temp.ns = store->check_context->ns;
			ident = &temp;
		}

		obj = scope_lookup(store->check_context->scope, ident);
		if (!obj) {
			obj = scope_lookup(store->check_context->scope,
				&atype->alias);
		}
		if (!obj) {
			if (atype->unwrap) {
				error(store->check_context, atype->loc,
					"Cannot unwrap unknown type alias %s",
					identifier_unparse(ident));
			}
			identifier_dup(&type->alias.ident, ident);
			type->alias.type = NULL;
			type->size = SIZE_UNDEFINED;
			type->align = SIZE_UNDEFINED;
			return;
		}

		if (obj->otype != O_TYPE) {
			error(store->check_context, atype->loc,
				"Object '%s' is not a type",
				identifier_unparse(&obj->ident));
			type->alias.type = NULL;
			type->size = SIZE_UNDEFINED;
			type->align = SIZE_UNDEFINED;
			return;
		}

		if (atype->unwrap) {
			*type = *type_dealias(obj->type);
			break;
		}

		identifier_dup(&type->alias.ident, &obj->ident);
		type->alias.type = obj->type;
		type->size = type->alias.type->size;
		type->align = type->alias.type->align;
		break;
	case STORAGE_ARRAY:
		type->array.length = ast_array_len(store, atype);
		type->array.members = type_store_lookup_atype(
				store, atype->array.members);
		if (type->array.members->size == SIZE_UNDEFINED) {
			type->align = SIZE_UNDEFINED;
			type->size = SIZE_UNDEFINED;
			error(store->check_context, atype->loc,
				"Array member must have defined size");
			return;
		}

		type->align = type->array.members->align;
		if (type->array.length == SIZE_UNDEFINED) {
			type->size = SIZE_UNDEFINED;
		} else {
			type->size = type->array.members->size * type->array.length;
		}
		break;
	case STORAGE_ENUM:
		type->_enum.storage = atype->_enum.storage;
		const struct type *storage =
			builtin_type_for_storage(type->_enum.storage, true);
		if (!type_is_integer(storage)) {
			error(store->check_context, atype->loc,
				"Enum storage must be an integer");
			// TODO: Use a dedicated error type
			*type = builtin_type_void;
			return;
		}
		type->size = storage->size;
		type->align = storage->size;

		struct scope *scope = scope_push(
			&store->check_context->scope, SCOPE_ENUM);
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
					avalue->value, &in, storage);
				enum eval_result r =
					eval_expr(store->check_context, &in, &out);
				if (r != EVAL_OK) {
					error(store->check_context, atype->loc,
						"Cannot evaluate enum value at compile time");
					continue;
				}
				if (!type_is_assignable(storage, out.result)) {
					error(store->check_context, atype->loc,
						"Cannotg assign enum value to enum type");
					continue;
				}
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

			struct identifier name = {
				.name = value->name,
				.ns = NULL,
			};
			// TODO: This leaks:
			struct expression *vexpr = xcalloc(1, sizeof(struct expression));
			vexpr->type = EXPR_CONSTANT;
			vexpr->result = storage;
			vexpr->constant.uval = value->uval;
			scope_insert(scope, O_CONST, &name, &name, storage, vexpr);

			values = &value->next;
			avalue = avalue->next;
		}
		scope_pop(&store->check_context->scope);
		scope_free(scope);
		break;
	case STORAGE_FUNCTION:
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
	case STORAGE_POINTER:
		type->size = 8; // XXX: ARCH
		type->align = 8;
		type->pointer.flags = atype->pointer.flags;
		type->pointer.referent = type_store_lookup_atype(
			store, atype->pointer.referent);
		break;
	case STORAGE_TYPE:
		type->size = 8; // XXX: ARCH
		type->align = 8;
		break;
	case STORAGE_SLICE:
		type->size = 24; // XXX: ARCH
		type->align = 8;
		type->array.members = type_store_lookup_atype(
			store, atype->array.members);
		type->array.length = SIZE_UNDEFINED;
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		type->struct_union.c_compat = true;
		struct_init_from_atype(store, type->storage, &type->size,
			&type->align, &type->struct_union.fields,
			&atype->struct_union, &type->struct_union.c_compat);
		if (!type->struct_union.c_compat) {
			// Recompute size
			type->size = 0;
			for (struct struct_field *f = type->struct_union.fields;
					f; f = f->next) {
				if (f->offset + f->type->size > type->size) {
					type->size = f->offset + f->type->size;
				}
			}
		}
		break;
	case STORAGE_TAGGED:
		tagged_init_from_atype(store, type, atype);
		break;
	case STORAGE_TUPLE:
		tuple_init_from_atype(store, type, atype);
		break;
	}
}

static const struct type *
_type_store_lookup_type(
	struct type_store *store,
	const struct type *type,
	bool recur)
{
	const struct type *builtin = builtin_for_type(type);
	if (builtin) {
		return builtin;
	}

	struct type psuedotype = *type;
	if (type->storage == STORAGE_ALIAS && !recur) {
		// References to type aliases always inherit the flags that the
		// alias was defined with
		//
		// TODO: This is likely problematic for forward references
		// TODO: Does this need a spec update?
		const struct scope_object *obj = scope_lookup(
			store->check_context->scope, &type->alias.ident);
		if (obj) {
			psuedotype.flags |= obj->type->flags;
			type = &psuedotype;
		}
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
	if (type->size != SIZE_UNDEFINED
			&& type->size != 0
			&& type->size % type->align != 0) {
		bucket->type.size += type->align - (type->size - type->align) % type->align;
	}
	return &bucket->type;
}

static const struct type *
type_store_lookup_type(struct type_store *store, const struct type *type)
{
	return _type_store_lookup_type(store, type, false);
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
		.storage = STORAGE_POINTER,
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
		.storage = STORAGE_ARRAY,
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
		.storage = STORAGE_SLICE,
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
	const struct identifier *ident, const struct type *secondary,
	bool exported)
{
	struct type alias = {
		.storage = STORAGE_ALIAS,
		.alias = {
			.ident = *ident,
			.type = secondary,
			.exported = exported,
		},
		.size = secondary->size,
		.align = secondary->align,
		.flags = secondary->flags,
	};
	// XXX: This needs to be updated on updates to type_flags (types.h)
	unsigned int flags[] = {
		0,
		TYPE_CONST,
		TYPE_ERROR,
		TYPE_ERROR | TYPE_CONST,
	};
	for (size_t i = 0; i < sizeof(flags) / sizeof(flags[0]); i++) {
		if ((flags[i] & secondary->flags) != secondary->flags) {
			continue;
		}
		const struct type *flagged = type_store_lookup_with_flags(store,
			secondary, flags[i]);
		alias.flags = flagged->flags;
		alias.alias.type = flagged;
		struct type *falias =
			(struct type *)type_store_lookup_type(store, &alias);
		if (falias->alias.type == NULL) {
			// Finish filling in forward referenced type
			falias->alias.type = type_store_lookup_with_flags(store,
				secondary, secondary->flags | flags[i]);
			falias->alias.exported = exported;
			falias->size = secondary->size;
			falias->align = secondary->align;
			falias->flags = secondary->flags | flags[i];
		}
	}
	alias.flags = secondary->flags;
	alias.alias.type = secondary;
	alias.alias.exported = exported;
	return type_store_lookup_type(store, &alias);
}

const struct type *
type_store_lookup_tagged(struct type_store *store,
		struct type_tagged_union *tags)
{
	struct type type = {
		.storage = STORAGE_TAGGED,
	};
	size_t nmemb = sum_tagged_memb(store, tags);
	struct type_tagged_union **tu =
		xcalloc(nmemb, sizeof(struct type_tagged_union *));
	size_t i = 0;
	collect_tagged_memb(store, tu, tags, &i);
	tagged_init(store, &type, tu, nmemb);
	return type_store_lookup_type(store, &type);
}

const struct type *
type_store_tagged_to_union(struct type_store *store, const struct type *tagged)
{
	assert(tagged->storage == STORAGE_TAGGED);
	struct type type = {
		.storage = STORAGE_UNION,
		.flags = tagged->flags,
	};
	struct struct_field **next = &type.struct_union.fields;
	for (const struct type_tagged_union *tu = &tagged->tagged;
			tu; tu = tu->next) {
		if (tu->type->size == 0) {
			continue;
		}
		assert(tu->type->size != SIZE_UNDEFINED);

		if (tu->type->size > type.size) {
			type.size = tu->type->size;
		}
		if (tu->type->align > type.align) {
			type.align = tu->type->align;
		}

		struct struct_field *sf =
			xcalloc(1, sizeof(struct struct_field));
		sf->name = "unnamed";
		sf->type = tu->type;
		sf->next = *next, *next = sf;
		next = &sf->next;
	}
	type.struct_union.c_compat = true; // XXX: Unsure about this
	return type_store_lookup_type(store, &type);
}

const struct type *
type_store_lookup_tuple(struct type_store *store, struct type_tuple *values)
{
	struct type type = {
		.storage = STORAGE_TUPLE,
		.tuple = *values,
	};
	for (struct type_tuple *t = values; t; t = t->next) {
		if (t->type->align > type.align) {
			type.align = t->type->align;
		}
		t->offset = type.size % t->type->align + type.size;
		type.size += type.size % t->type->align + t->type->size;
	}
	return type_store_lookup_type(store, &type);
}

const struct type *
type_store_type(struct type_store *store)
{
	struct type type = {
		.storage = STORAGE_TYPE,
		.size = 8, // XXX: ARCH
		.align = 8,
	};
	return type_store_lookup_type(store, &type);
}

// Algorithm:
// - Deduplicate and collect nested unions
// - Merge *type with nullable *type
// - If one of the types is null:
// 	- If there's more than one pointer type, error out
// 	- If there's one pointer type, make it nullable and drop the null
// 	- If there are no pointer types, keep the null
// - If the resulting union only has one type, return that type
// - Otherwise, return a tagged union of all the selected types
const struct type *
type_store_reduce_result(struct type_store *store, struct type_tagged_union *in)
{
	if (!in) {
		return &builtin_type_void;
	} else if (!in->next) {
		return in->type;
	}

	const struct type *type = type_store_lookup_tagged(store, in);
	struct type_tagged_union _in = type->tagged;
	in = &_in;

	struct type_tagged_union **null = NULL;
	struct type_tagged_union *ptr = NULL;
	bool multiple_ptrs = false;
	struct type_tagged_union **tu = &in;
	while (*tu != NULL) {
		struct type_tagged_union *i = *tu;
		bool dropped = false;
		const struct type *it = i->type;

		if (it->flags & TYPE_CONST) {
			struct type_tagged_union **j = &in;
			while (*j) {
				const struct type *jt = (*j)->type;
				if (jt == it) {
					j = &(*j)->next;
					continue;
				}
				jt = type_store_lookup_with_flags(store, jt,
					jt->flags | TYPE_CONST);
				if (jt == it) {
					*j = (*j)->next;
				} else {
					j = &(*j)->next;
				}
			}
		}

		for (struct type_tagged_union *j = in; j != i; j = j->next) {
			const struct type *jt = j->type;
			assert(it->id != jt->id);
			if (it->storage != STORAGE_POINTER
					|| jt->storage != STORAGE_POINTER) {
				continue;
			}
			if (it->pointer.referent->id != jt->pointer.referent->id) {
				continue;
			}
			if (it->flags != jt->flags) {
				continue;
			}
			if ((it->pointer.flags & PTR_NULLABLE)
					|| (jt->pointer.flags & PTR_NULLABLE)) {
				it = type_store_lookup_pointer(store,
					it->pointer.referent, PTR_NULLABLE);
				jt = type_store_lookup_pointer(store,
					jt->pointer.referent, PTR_NULLABLE);
				if (it == jt) {
					dropped = true;
					*tu = i->next;
					j->type = jt;
					break;
				}
			};
		}

		if (i->type->storage == STORAGE_NULL) {
			null = tu;
		}
		if (!dropped) {
			if (i->type->storage == STORAGE_POINTER) {
				if (ptr != NULL) {
					multiple_ptrs = true;
				}
				ptr = i;
			}
			tu = &i->next;
		}
	}

	if (null != NULL && (multiple_ptrs || ptr == NULL)) {
		return NULL;
	}

	if (null != NULL && ptr != NULL) {
		*null = (*null)->next;
		ptr->type = type_store_lookup_pointer(store,
			ptr->type->pointer.referent, PTR_NULLABLE);
	}

	if (in->next == NULL) {
		return in->type;
	}
	return type_store_lookup_tagged(store, in);
}
