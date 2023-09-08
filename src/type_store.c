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
#include "types.h"
#include "util.h"

// XXX: This needs to be updated on updates to type_flags (types.h)
static const unsigned int typeflags[] = {
	0,
	TYPE_CONST,
	TYPE_ERROR,
	TYPE_ERROR | TYPE_CONST,
};

static struct dimensions lookup_atype_with_dimensions(struct type_store *store,
		const struct type **type, const struct ast_type *atype);

static const struct type *
lookup_atype(struct type_store *store, const struct ast_type *atype);

static void
error(struct context *ctx, const struct location loc, const char *fmt, ...)
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
	if (!type_is_integer(store->check_context, out.result)) {
		error(store->check_context, atype->loc,
			"Array length must be an integer");
		return SIZE_UNDEFINED;
	}
	if (type_is_signed(store->check_context, out.result) && out.constant.ival <= 0) {
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
	case STORAGE_ERROR:
		return &builtin_type_error;
	case STORAGE_F32:
		return is_const ? &builtin_type_const_f32 : &builtin_type_f32;
	case STORAGE_F64:
		return is_const ? &builtin_type_const_f64 : &builtin_type_f64;
	case STORAGE_I8:
		return is_const ? &builtin_type_const_i8 : &builtin_type_i8;
	case STORAGE_I16:
		return is_const ? &builtin_type_const_i16 : &builtin_type_i16;
	case STORAGE_I32:
		return is_const ? &builtin_type_const_i32 : &builtin_type_i32;
	case STORAGE_I64:
		return is_const ? &builtin_type_const_i64 : &builtin_type_i64;
	case STORAGE_INT:
		return is_const ? &builtin_type_const_int : &builtin_type_int;
	case STORAGE_NEVER:
		return is_const ? &builtin_type_const_never : &builtin_type_never;
	case STORAGE_OPAQUE:
		return is_const ? &builtin_type_const_opaque : &builtin_type_opaque;
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
	case STORAGE_VALIST:
		return &builtin_type_valist;
	case STORAGE_VOID:
		return is_const ? &builtin_type_const_void : &builtin_type_void;
	case STORAGE_NULL:
		return &builtin_type_null; // const null and null are the same type
	case STORAGE_STRING:
		return is_const ? &builtin_type_const_str : &builtin_type_str;
	case STORAGE_ALIAS:
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_RCONST:
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
	size_t *offset, const struct ast_struct_union_type *atype,
	const struct ast_struct_union_field *afield,
	bool *ccompat, bool size_only, bool last)
{
	// XXX: fuck linked lists all my homies hate linked lists
	while (*fields && (!afield->name || !(*fields)->name
			|| strcmp((*fields)->name, afield->name) != 0)) {
		fields = &(*fields)->next;
	}
	if (*fields != NULL) {
		assert(afield->name != NULL);
		error(store->check_context, afield->type->loc,
			"Duplicate struct/union member '%s'", afield->name);
		return NULL;
	}
	// XXX: leaks if size_only
	*fields = xcalloc(1, sizeof(struct struct_field));
	struct struct_field *field = *fields;

	if (afield->name) {
		field->name = xstrdup(afield->name);
	}
	struct dimensions dim = {0};
	if (size_only) {
		dim = lookup_atype_with_dimensions(store, NULL, afield->type);
	} else {
		dim = lookup_atype_with_dimensions(store, &field->type, afield->type);
	}
	if (dim.size == 0) {
		error(store->check_context, afield->type->loc,
			"Type of size 0 is not a valid struct/union member");
		return NULL;
	}
	if (!last && dim.size == SIZE_UNDEFINED) {
		error(store->check_context, afield->type->loc,
			"Type of undefined size is not a valid struct/union member");
		return NULL;
	}
	if (dim.align == ALIGN_UNDEFINED) {
		error(store->check_context, afield->type->loc,
			"Type of undefined alignment is not a valid struct/union member");
		return NULL;
	}
	assert(dim.align != 0);

	if (afield->offset) {
		*ccompat = false;
		struct expression in, out;
		check_expression(store->check_context, afield->offset, &in, NULL);
		field->offset = *offset;
		enum eval_result r = eval_expr(store->check_context, &in, &out);
		if (r != EVAL_OK) {
			error(store->check_context, in.loc,
				"Cannot evaluate field offset at compile time");
		} else if (!type_is_integer(store->check_context, out.result)) {
			error(store->check_context, in.loc,
				"Field offset must be an integer");
		} else if (type_is_signed(store->check_context, out.result) && out.constant.ival < 0) {
			error(store->check_context, in.loc,
				"Field offset must not be less than 0");
		} else if (out.constant.uval < *offset) {
			error(store->check_context, in.loc,
				"Field offset must be greater than or equal to previous field's offset");
		} else if (out.constant.uval < *size) {
			error(store->check_context, in.loc,
				"Fields must not have overlapping storage");
		} else {
			field->offset = *offset = (size_t)out.constant.uval;
		}
	} else if (atype->packed) {
		field->offset = *offset = *size;
	} else {
		*offset = *size;
		if (*offset % dim.align) {
			*offset += dim.align - (*offset % dim.align);
		}
		field->offset = *offset;
		assert(field->offset % dim.align == 0);
	}

	if (dim.size == SIZE_UNDEFINED || *size == SIZE_UNDEFINED) {
		*size = SIZE_UNDEFINED;
	} else if (storage == STORAGE_STRUCT) {
		*size = field->offset + dim.size;
	} else {
		*usize = dim.size > *usize ? dim.size : *usize;
	}
	*align = dim.align > *align ? dim.align : *align;
	field->size = dim.size;
	return field;
}

static const struct type *type_store_lookup_type(struct type_store *store, const struct type *type);

void
shift_fields(struct type_store *store,
	const struct ast_struct_union_field *afield, struct struct_field *parent)
{
	if (parent->type->storage == STORAGE_ALIAS
			&& type_dealias(store->check_context, parent->type)->storage != STORAGE_STRUCT
			&& type_dealias(store->check_context, parent->type)->storage != STORAGE_UNION) {
		assert(afield);
		error(store->check_context, afield->type->loc,
			"Cannot embed non-struct non-union alias");
		parent->type = &builtin_type_error;
		return;
	}
	if (parent->offset == 0) {
		// We need to return early here in order to avoid dealiasing an
		// embedded alias. This is acceptable at nonzero offsets, but we
		// need to keep the alias if it's at offset 0 because of
		// subtyping.
		return;
	}
	const struct type *type = type_dealias(store->check_context, parent->type);
	assert(type->storage == STORAGE_STRUCT
		|| type->storage == STORAGE_UNION);
	struct type new = {
		.storage = type->storage,
		.flags = type->flags,
		.size = type->size,
		.align = type->align,
		.struct_union.c_compat = type->struct_union.c_compat,
		.struct_union.packed = type->struct_union.packed,
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
			new->name = xstrdup(field->name);
		} else {
			shift_fields(store, NULL, new);
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
	const struct ast_struct_union_type *atype, bool *ccompat, bool size_only)
{
	// TODO: fields with size SIZE_UNDEFINED
	size_t usize = 0;
	size_t offset = 0;
	assert(storage == STORAGE_STRUCT || storage == STORAGE_UNION);
	const struct ast_struct_union_field *afield = &atype->fields;
	while (afield) {
		bool last = afield->next == NULL;
		struct struct_field *field = struct_insert_field(store, fields,
			storage, size, &usize, align, &offset, atype, afield,
			ccompat, size_only, last);
		if (field == NULL) {
			return;
		}
		if (!field->name && !size_only) {
			// We need to shift the embedded struct/union's fields
			// so that their offsets are from the start of the
			// parent type. This is a bit of a hack, but it makes
			// type_get_field far easier to implement and doesn't
			// cause any trouble in gen since offsets are only used
			// there for sorting fields.
			shift_fields(store, afield, field);
		}
		afield = afield->next;
	}

	if (storage == STORAGE_UNION) {
		*size = usize;
	}
}

static bool
enforce_tagged_invariants(struct type_store *store, struct location loc,
		const struct type *type)
{
	int i;
	const struct type_tagged_union *tu;
	for (i = 0, tu = &type->tagged; tu; i++, tu = tu->next) {
		if (tu->type->storage == STORAGE_NULL) {
			error(store->check_context, loc,
				"Null type not allowed in this context");
			return false;
		}
		if (tu->type->size == SIZE_UNDEFINED) {
			error(store->check_context, loc,
				"Type of undefined size is not a valid tagged union member");
			return false;
		}
		assert(tu->type->align != ALIGN_UNDEFINED);
	}
	if (i <= 1) {
		error(store->check_context, loc,
			"Tagged unions must have at least two distinct members");
		return false;
	}
	return true;
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

// get next member of an incomplete tagged union without completing it
static void
tagged_or_atagged_member(struct type_store *store,
		const struct ast_type **atype, const struct type **type)
{
	const struct ast_type *_atype = *atype;
	while (_atype->storage == STORAGE_ALIAS && _atype->unwrap) {
		const struct scope_object *obj = scope_lookup(
			store->check_context->scope, &_atype->alias);
		if (!obj) {
			error(store->check_context, _atype->loc,
				"Unknown object '%s'",
				identifier_unparse(&_atype->alias));
			*type = &builtin_type_error;
			return;
		}
		if (obj->otype != O_SCAN) {
			if (obj->otype == O_TYPE) {
				*type = type_dealias(store->check_context, obj->type);
				return;
			} else {
				error(store->check_context, _atype->loc,
					"Object '%s' is not a type",
					identifier_unparse(&obj->ident));
				*type = &builtin_type_error;
				return;
			}
		}
		struct incomplete_declaration *idecl =
			(struct incomplete_declaration *)obj;
		if (idecl->type != IDECL_DECL
				|| idecl->decl.decl_type != ADECL_TYPE) {
			error(store->check_context, _atype->loc,
				"Object '%s' is not a type",
				identifier_unparse(&obj->ident));
			*type = &builtin_type_error;
			return;
		}
		_atype = idecl->decl.type.type;
	}
	*type = NULL;
	*atype = _atype;
}

static size_t
sum_atagged_memb(struct type_store *store,
		const struct ast_tagged_union_type *u)
{
	size_t nmemb = 0;
	for (; u; u = u->next) {
		const struct type *type = NULL;
		const struct ast_type *atype = u->type;
		tagged_or_atagged_member(store, &atype, &type);
		if (type != NULL && type->storage == STORAGE_TAGGED) {
			nmemb += sum_tagged_memb(store, &type->tagged);
		} else if (atype->storage == STORAGE_TAGGED) {
			nmemb += sum_atagged_memb(store, &atype->tagged_union);
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
		tu->type = lower_const(store->check_context, type, NULL);
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
		const struct type *type = lookup_atype(store, atu->type);
		if (type->storage == STORAGE_TAGGED) {
			collect_tagged_memb(store, ta, &type->tagged, i);
			continue;
		}
		struct type_tagged_union *tu;
		ta[*i] = tu = xcalloc(1, sizeof(struct type_tagged_union));
		tu->type = lower_const(store->check_context, type, NULL);
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

	if (type->align < builtin_type_u32.align) {
		type->align = builtin_type_u32.align;
	}
	type->size += builtin_type_u32.size % type->align
		+ builtin_type_u32.align;
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
	tagged_init(store, type, tu, nmemb);
	if (!enforce_tagged_invariants(store, atype->loc, type)) {
		*type = builtin_type_error;
	};
}

static struct dimensions
_tagged_size(struct type_store *store, const struct ast_tagged_union_type *u)
{
	struct dimensions dim = {0};
	for (; u; u = u->next) {
		struct dimensions memb = {0};
		const struct type *type = NULL;
		const struct ast_type *atype = u->type;
		tagged_or_atagged_member(store, &atype, &type);
		if (type != NULL && type->storage == STORAGE_TAGGED) {
			for (const struct type_tagged_union *u = &type->tagged;
					u; u = u->next) {
				if (memb.size < u->type->size) {
					memb.size = u->type->size;
				}
				if (memb.align < u->type->align) {
					memb.align = u->type->align;
				}
			}
		} else if (atype->storage == STORAGE_TAGGED) {
			memb = _tagged_size(store, &atype->tagged_union);
		} else {
			memb = lookup_atype_with_dimensions(store, NULL, atype);
		}
		if (memb.size == SIZE_UNDEFINED) {
			error(store->check_context, atype->loc,
				"Type of undefined size is not a valid tagged union member");
			return (struct dimensions){0};
		}
		if (dim.size < memb.size) {
			dim.size = memb.size;
		}
		if (dim.align < memb.align) {
			dim.align = memb.align;
		}
	}
	return dim;
}

// compute the dimensions of an incomplete tagged union without completing it
static struct dimensions
tagged_size(struct type_store *store, const struct ast_type *atype)
{
	struct dimensions dim = _tagged_size(store, &atype->tagged_union);
	if (dim.align < builtin_type_u32.align) {
		dim.align = builtin_type_u32.align;
	}
	dim.size += builtin_type_u32.size % dim.align + builtin_type_u32.align;
	return dim;
}


static struct dimensions
tuple_init_from_atype(struct type_store *store,
	struct type *type, const struct ast_type *atype)
{
	const struct ast_tuple_type *atuple = &atype->tuple;
	struct type_tuple *cur = NULL;
	if (type) {
		type->size = 0, type->align = 0;
		cur = &type->tuple;
	}
	struct dimensions dim = {0};
	while (atuple) {
		struct dimensions memb = {0};
		size_t offset = 0;
		if (type) {
			memb = lookup_atype_with_dimensions(store, &cur->type, atuple->type);
		} else {
			memb = lookup_atype_with_dimensions(store, NULL, atuple->type);
		}
		if (memb.size == 0) {
			error(store->check_context, atype->loc,
				"Type of size 0 is not a valid tuple member");
			if (type) {
				*type = builtin_type_error;
			}
			return (struct dimensions){0};
		}
		if (memb.size == SIZE_UNDEFINED) {
			error(store->check_context, atype->loc,
				"Type of undefined size is not a valid tuple member");
			if (type) {
				*type = builtin_type_error;
			}
			return (struct dimensions){0};
		}
		offset = dim.size % memb.align + dim.size;
		dim.size += dim.size % memb.align + memb.size;
		if (dim.align < memb.align) {
			dim.align = memb.align;
		}

		atuple = atuple->next;
		if (type) {
			cur->offset = offset;
			if (atuple) {
				cur->next = xcalloc(1, sizeof(struct type_tuple));
				cur = cur->next;
			}
		}
	}
	if (type) {
		type->size = dim.size;
		type->align = dim.align;
	}
	return dim;
}

static const struct type *
type_store_lookup_type(struct type_store *store, const struct type *type);

static void
add_padding(size_t *size, size_t align)
{
	if (*size != SIZE_UNDEFINED && *size != 0 && *size % align != 0) {
		*size += align - (*size - align) % align;
	}
}

static struct dimensions
type_init_from_atype(struct type_store *store,
	struct type *type,
	const struct ast_type *atype)
{
	struct type tmp = {0};
	bool size_only = false;
	if (type == NULL) {
		type = &tmp;
		size_only = true;
	}

	type->storage = atype->storage;
	type->flags = atype->flags;

	const struct scope_object *obj = NULL;
	const struct type *builtin;
	switch (type->storage) {
	case STORAGE_ERROR:
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_RCONST:
	case STORAGE_ENUM:
	case STORAGE_NULL:
		assert(0); // Invariant
	case STORAGE_BOOL:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_STRING:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VALIST:
	case STORAGE_VOID:
		builtin = builtin_type_for_storage(type->storage, false);
		type->size = builtin->size;
		type->align = builtin->align;
		break;
	case STORAGE_ALIAS:
		obj = scope_lookup(store->check_context->scope, &atype->alias);
		if (!obj) {
			error(store->check_context, atype->loc,
				"Unresolvable identifier '%s'",
				identifier_unparse(&atype->alias));
			*type = builtin_type_error;
			return (struct dimensions){0};
		}

		if (obj->otype == O_SCAN) {
			// an incomplete declaration was encountered
			struct incomplete_declaration *idecl =
				(struct incomplete_declaration *)obj;
			if (size_only && idecl->type == IDECL_DECL) {
				wrap_resolver(store->check_context, obj,
						resolve_dimensions);
				type->size = obj->type->size;
				type->align = obj->type->align;
				break;
			}
			// complete it first and then proceed normally
			wrap_resolver(store->check_context, obj, resolve_type);
		}

		if (obj->otype != O_TYPE) {
			error(store->check_context, atype->loc,
				"Object '%s' is not a type",
				identifier_unparse(&obj->ident));
			*type = builtin_type_error;
			return (struct dimensions){0};
		}

		type->storage = obj->type->storage;
		if (obj->type->storage == STORAGE_ENUM) {
			type->_enum = obj->type->_enum;
		} else if (atype->unwrap) {
			*type = *type_dealias(store->check_context, obj->type);
			break;
		}
		identifier_dup(&type->alias.ident, &obj->ident);
		identifier_dup(&type->alias.name, &obj->name);
		type->alias.type = obj->type->alias.type;
		type->alias.exported = obj->type->alias.exported;
		type->size = obj->type->size;
		type->align = obj->type->align;
		break;
	case STORAGE_ARRAY:
		type->array.length = ast_array_len(store, atype);
		struct dimensions memb = {0};
		if (size_only) {
			memb = lookup_atype_with_dimensions(store,
				NULL, atype->array.members);
		} else {
			memb = lookup_atype_with_dimensions(store,
				&type->array.members, atype->array.members);
		}
		// XXX: I'm not sure these checks are *exactly* right, we might
		// still be letting some invalid stuff through
		if (type->array.length != SIZE_UNDEFINED && memb.size == 0) {
			error(store->check_context, atype->loc,
				"Type of size 0 is not a valid array member");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		if (memb.size == SIZE_UNDEFINED) {
			error(store->check_context, atype->loc,
				"Type of undefined size is not a valid array member");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}

		type->align = memb.align;
		if (type->array.length == SIZE_UNDEFINED) {
			type->size = SIZE_UNDEFINED;
		} else {
			type->size = memb.size * type->array.length;
		}
		break;
	case STORAGE_FUNCTION:
		type->size = SIZE_UNDEFINED;
		type->align = ALIGN_UNDEFINED;
		if (size_only) {
			break;
		}
		type->func.result = lookup_atype(store,
				atype->func.result);
		type->func.variadism = atype->func.variadism;
		struct type_func_param *param, **next = &type->func.params;
		for (struct ast_function_parameters *aparam = atype->func.params;
				aparam; aparam = aparam->next) {
			param = *next = xcalloc(1, sizeof(struct type_func_param));
			param->type = lookup_atype(store, aparam->type);
			if (param->type->size == 0) {
				error(store->check_context, atype->loc,
					"Function parameter types must have nonzero size");
				*type = builtin_type_error;
				return (struct dimensions){0};
			}
			if (param->type->size == SIZE_UNDEFINED) {
				error(store->check_context, atype->loc,
					"Function parameter types must have defined size");
				*type = builtin_type_error;
				return (struct dimensions){0};
			}
			if (atype->func.variadism == VARIADISM_HARE
					&& !aparam->next) {
				param->type = type_store_lookup_slice(
					store, aparam->loc, param->type);
			}
			next = &param->next;
		}
		break;
	case STORAGE_POINTER:
		type->size = builtin_type_uintptr.size;
		type->align = builtin_type_uintptr.align;
		if (size_only) {
			break;
		}
		type->pointer.flags = atype->pointer.flags;
		type->pointer.referent = lookup_atype(
			store, atype->pointer.referent);
		if (type->pointer.referent->size == 0) {
			error(store->check_context, atype->loc,
				"Can't have pointer to zero-sized type");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		if (type->pointer.referent->storage == STORAGE_NEVER) {
			error(store->check_context, atype->loc,
				"Can't have pointer to never");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		break;
	case STORAGE_SLICE:
		type->size = builtin_type_uintptr.size
			+ 2 * builtin_type_size.size;
		type->align = builtin_type_uintptr.align;
		if (size_only) {
			break;
		}
		type->array.members = lookup_atype(
			store, atype->array.members);
		if (type->array.members->size == 0) {
			error(store->check_context, atype->loc,
				"Type of size 0 is not a valid slice member");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		type->array.length = SIZE_UNDEFINED;
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		type->struct_union.c_compat = !atype->struct_union.packed;
		type->struct_union.packed = atype->struct_union.packed;
		struct_init_from_atype(store, type->storage, &type->size,
			&type->align, &type->struct_union.fields,
			&atype->struct_union, &type->struct_union.c_compat,
			size_only);
		if (!type->struct_union.c_compat) {
			// Recompute size
			type->size = 0;
			for (struct struct_field *f = type->struct_union.fields;
					f; f = f->next) {
				if (f->type) assert(f->type->size == f->size);
				if (f->offset + f->size > type->size) {
					type->size = f->offset + f->size;
				}
			}
		}
		break;
	case STORAGE_TAGGED:
		if (size_only) {
			struct dimensions tagged = tagged_size(store, atype);
			type->size = tagged.size;
			type->align = tagged.align;
		} else {
			tagged_init_from_atype(store, type, atype);
		}
		break;
	case STORAGE_TUPLE:
		if (size_only) {
			struct dimensions tup;
			tup = tuple_init_from_atype(store, NULL, atype);
			type->size = tup.size;
			type->align = tup.align;
		} else {
			tuple_init_from_atype(store, type, atype);
		}
		break;
	}

	bool packed = false;
	if (type_is_complete(type)) {
		const struct type *final = type_dealias(store->check_context, type);
		if (final->storage == STORAGE_STRUCT) {
			packed = final->struct_union.packed;
		}
	}

	struct dimensions dim = {
		.size = type->size,
		.align = type->align,
	};
	if (!packed) {
		add_padding(&dim.size, dim.align);
	}
	return dim;
}

static const struct type *
_type_store_lookup_type(
	struct type_store *store,
	const struct type *type,
	const struct dimensions *dims)
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
			if (bucket->type.storage == STORAGE_ALIAS) {
				bucket->type.alias.type = type->alias.type;
			}
			return &bucket->type;
		}
		next = &bucket->next;
	}

	bucket = *next = xcalloc(1, sizeof(struct type_bucket));
	bucket->type = *type;
	bucket->type.id = hash;

	if (dims == NULL) {
		add_padding(&bucket->type.size, type->align);
	}

	return &bucket->type;
}

static const struct type *
type_store_lookup_type(struct type_store *store, const struct type *type)
{
	if (type->storage != STORAGE_ALIAS) {
		return _type_store_lookup_type(store, type, NULL);
	}
	// References to type aliases always inherit the flags that the
	// alias was defined with
	struct type psuedotype = *type;
	const struct scope_object *obj = scope_lookup(
		store->check_context->scope, &type->alias.name);
	psuedotype.flags |= obj->type->flags;
	return type_store_lookup_alias(store, &psuedotype, NULL);
}

static struct dimensions
lookup_atype_with_dimensions(struct type_store *store, const struct type **type, const struct ast_type *atype)
{
	struct type temp = {0};
	struct dimensions dim = {0};
	if (type) {
		dim = type_init_from_atype(store, &temp, atype);
		*type = type_store_lookup_type(store, &temp);
	} else {
		dim = type_init_from_atype(store, NULL, atype);
	}
	return dim;
}

static const struct type *
lookup_atype(struct type_store *store, const struct ast_type *atype)
{
	const struct type *type = NULL;
	lookup_atype_with_dimensions(store, &type, atype);
	return type;
}

const struct type *
type_store_lookup_atype(struct type_store *store, const struct ast_type *atype)
{
	if (atype->storage == STORAGE_NULL) {
		return &builtin_type_null;
	};
	return lookup_atype(store, atype);
}

// Compute dimensions of an incomplete type without completing it
struct dimensions
type_store_lookup_dimensions(struct type_store *store, const struct ast_type *atype)
{
	return type_init_from_atype(store, NULL, atype);
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
	return _type_store_lookup_type(store, &new, NULL);
}

const struct type *
type_store_lookup_pointer(struct type_store *store, struct location loc,
	const struct type *referent, unsigned int ptrflags)
{
	if (referent->storage == STORAGE_ERROR) {
		return &builtin_type_error;
	}
	if (referent->storage == STORAGE_NULL) {
		error(store->check_context, loc,
			"Null type not allowed in this context");
		return &builtin_type_error;
	}
	if (referent->size == 0) {
		error(store->check_context, loc,
			"Can't have pointer to zero-sized type");
		return &builtin_type_error;
	}
	if (referent->storage == STORAGE_NEVER) {
		error(store->check_context, loc,
			"Can't have pointer to never");
		return &builtin_type_error;
	}
	referent = lower_const(store->check_context, referent, NULL);

	struct type ptr = {
		.storage = STORAGE_POINTER,
		.pointer = {
			.referent = referent,
			.flags = ptrflags,
		},
		.size = builtin_type_uintptr.size,
		.align = builtin_type_uintptr.align,
	};
	return type_store_lookup_type(store, &ptr);
}

const struct type *
type_store_lookup_array(struct type_store *store, struct location loc,
	const struct type *members, size_t len, bool expandable)
{
	if (members->storage == STORAGE_ERROR) {
		return &builtin_type_error;
	}
	if (members->storage == STORAGE_NULL) {
		error(store->check_context, loc,
			"Null type not allowed in this context");
		return &builtin_type_error;
	}
	members = lower_const(store->check_context, members, NULL);
	// XXX: I'm not sure these checks are *exactly* right, we might still
	// be letting some invalid stuff pass
	if (len != SIZE_UNDEFINED && members->size == 0) {
		error(store->check_context, loc,
			"Type of size 0 is not a valid array member");
		return &builtin_type_error;
	}
	if (members->size == SIZE_UNDEFINED) {
		error(store->check_context, loc,
			"Type of undefined size is not a valid member of a bounded array");
		return &builtin_type_error;
	}
	assert(members->align != 0);
	assert(members->align != ALIGN_UNDEFINED);

	struct type array = {
		.storage = STORAGE_ARRAY,
		.array = {
			.members = members,
			.length = len,
			// TODO: Define expandable semantics better in spec
			.expandable = expandable,
		},
		.size = len == SIZE_UNDEFINED
			? SIZE_UNDEFINED : members->size * len,
		.align = members->align,
	};
	return type_store_lookup_type(store, &array);
}

const struct type *
type_store_lookup_slice(struct type_store *store, struct location loc,
	const struct type *members)
{
	if (members->storage == STORAGE_ERROR) {
		return &builtin_type_error;
	}
	if (members->storage == STORAGE_NULL) {
		error(store->check_context, loc,
			"Null type not allowed in this context");
		return &builtin_type_error;
	}
	members = lower_const(store->check_context, members, NULL);
	if (members->size == 0) {
		error(store->check_context, loc,
			"Type of size 0 is not a valid slice member");
		return &builtin_type_error;
	}
	assert(members->align != 0);

	struct type slice = {
		.storage = STORAGE_SLICE,
		.array = {
			.members = members,
			.length = SIZE_UNDEFINED,
		},
		.size = builtin_type_uintptr.size + 2 * builtin_type_size.size,
		.align = builtin_type_uintptr.align,
	};
	return type_store_lookup_type(store, &slice);
}

const struct type *
type_store_lookup_alias(struct type_store *store,
		const struct type *type,
		const struct dimensions *dims)
{
	struct type tmp = *type;
	const struct type *ret = NULL;
	for (size_t i = 0; i < sizeof(typeflags) / sizeof(typeflags[0]); i++) {
		if ((typeflags[i] & type->flags) != type->flags) {
			continue;
		}
		if (type->alias.type) {
			tmp.alias.type = type_store_lookup_with_flags(
				store, type->alias.type, typeflags[i]);
		}
		tmp.flags = typeflags[i];
		const struct type *alias = _type_store_lookup_type(
				store, &tmp, dims);
		if (typeflags[i] == type->flags) {
			ret = alias;
		}
	}
	assert(ret != NULL);
	return ret;
}


// Sorts members by id and deduplicates entries. Does not enforce usual tagged
// union invariants. The returned type is not a singleton.
static const struct type *
lookup_tagged(struct type_store *store, struct type_tagged_union *tags)
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
	struct type *ret = xcalloc(1, sizeof(struct type));
	*ret = type;
	return ret;
}

const struct type *
type_store_lookup_tagged(struct type_store *store, struct location loc,
		struct type_tagged_union *tags)
{
	const struct type *type = lookup_tagged(store, tags);
	if (!enforce_tagged_invariants(store, loc, type)) {
		return &builtin_type_error;
	}
	return type_store_lookup_type(store, type);
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
		if (tu->type->storage == STORAGE_ERROR) {
			return &builtin_type_error;
		}
		if (tu->type->size == 0) {
			continue;
		}

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
type_store_lookup_tuple(struct type_store *store, struct location loc,
		struct type_tuple *values)
{
	struct type type = {
		.storage = STORAGE_TUPLE,
	};
	for (struct type_tuple *t = values; t; t = t->next) {
		if (t->type->storage == STORAGE_ERROR) {
			return &builtin_type_error;
		}
		if (t->type->storage == STORAGE_NULL) {
			error(store->check_context, loc,
				"Null type not allowed in this context");
			return &builtin_type_error;
		}
		t->type = lower_const(store->check_context, t->type, NULL);
		if (t->type->size == 0) {
			error(store->check_context, loc,
				"Type of size 0 is not a valid tuple member");
			return &builtin_type_error;
		}
		if (t->type->size == SIZE_UNDEFINED) {
			error(store->check_context, loc,
				"Type of undefined size is not a valid tuple member");
			return &builtin_type_error;
		}
		assert(t->type->align != 0);
		assert(t->type->align != ALIGN_UNDEFINED);

		if (t->type->align > type.align) {
			type.align = t->type->align;
		}
		t->offset = type.size % t->type->align + type.size;
		type.size += type.size % t->type->align + t->type->size;
	}
	type.tuple = *values;
	return type_store_lookup_type(store, &type);
}

const struct type *
type_store_lookup_enum(struct type_store *store, const struct ast_type *atype,
	bool exported)
{
	struct type type = {0};
	type.storage = STORAGE_ENUM;
	type.flags = atype->flags;
	mkident(store->check_context, &type.alias.ident, &atype->alias, NULL);
	identifier_dup(&type.alias.name, &atype->alias);
	type.alias.exported = exported;
	type.alias.type =
		builtin_type_for_storage(atype->_enum.storage, false);
	if (!type_is_integer(store->check_context, type.alias.type)
			&& type.alias.type->storage != STORAGE_RUNE) {
		error(store->check_context, atype->loc,
			"Enum storage must be an integer or rune");
		return &builtin_type_error;
	}
	type.size = type.alias.type->size;
	type.align = type.alias.type->size;
	return type_store_lookup_type(store, &type);
}

// Algorithm:
// - Deduplicate and collect nested unions
// - Remove never
// - Merge *type with nullable *type
// - If one of the types is null:
// 	- If there's more than one pointer type, error out
// 	- If there's one pointer type, make it nullable and drop the null
// 	- If there are no pointer types, keep the null
// - If the resulting union only has one type, return that type
// - Otherwise, if no types remain, return never
// - Otherwise, return a tagged union of all the selected types
const struct type *
type_store_reduce_result(struct type_store *store, struct location loc,
		struct type_tagged_union *in)
{
	if (!in) {
		return &builtin_type_never;
	} else if (!in->next) {
		return in->type;
	}

	const struct type *type = lookup_tagged(store, in);
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

		if (it->storage == STORAGE_NEVER || it->storage == STORAGE_ERROR) {
			*tu = i->next;
			continue;
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
				it = type_store_lookup_pointer(store, loc,
					it->pointer.referent, PTR_NULLABLE);
				jt = type_store_lookup_pointer(store, loc,
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
		ptr->type = type_store_lookup_pointer(store, loc,
			ptr->type->pointer.referent, PTR_NULLABLE);
	}

	if (in == NULL) {
		return &builtin_type_never;
	} else if (in->next == NULL) {
		return in->type;
	}
	return type_store_lookup_tagged(store, loc, in);
}
