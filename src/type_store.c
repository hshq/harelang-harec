#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "check.h"
#include "eval.h"
#include "scope.h"
#include "type_store.h"
#include "types.h"
#include "util.h"

static struct dimensions lookup_atype_with_dimensions(struct context *ctx,
		const struct type **type, const struct ast_type *atype);

static const struct type *
lookup_atype(struct context *ctx, const struct ast_type *atype);

static size_t
ast_array_len(struct context *ctx, const struct ast_type *atype)
{
	// TODO: Maybe we should cache these
	struct expression in, out;
	if (atype->array.length == NULL) {
		return SIZE_UNDEFINED;
	}
	check_expression(ctx, atype->array.length, &in, NULL);
	if (!eval_expr(ctx, &in, &out)) {
		error(ctx, atype->loc, NULL,
			"Cannot evaluate array length at compile time");
		return SIZE_UNDEFINED;
	}
	if (!type_is_integer(ctx, out.result)) {
		error(ctx, atype->loc, NULL, "Array length must be an integer");
		return SIZE_UNDEFINED;
	}
	if (type_is_signed(ctx, out.result) && out.literal.ival < 0) {
		error(ctx, atype->loc, NULL,
			"Array length must be non-negative");
		return SIZE_UNDEFINED;
	}
	return (size_t)out.literal.uval;
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
	case STORAGE_DONE:
		return is_const ? &builtin_type_const_done : &builtin_type_done;
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

static bool
struct_union_has_field(struct context *ctx,
	const char *name,
	const struct struct_field *fields)
{
	for (; fields; fields = fields->next) {
		if (fields->name != NULL) {
			if (strcmp(fields->name, name) == 0) {
				return true;
			}
			continue;
		}

		assert(fields->type != NULL);
		const struct type *type = type_dealias(ctx, fields->type);
		if (struct_union_has_field(ctx, name, type->struct_union.fields)) {
			return true;
		}
	}

	return false;
}

static struct struct_field *
struct_new_field(struct context *ctx, struct type *type,
	const struct ast_struct_union_field *afield,
	size_t *usize, size_t *offset, bool size_only)
{
	if (afield->name != NULL && !size_only) {
		if (struct_union_has_field(ctx, afield->name, type->struct_union.fields)) {
			error(ctx, afield->type->loc, NULL,
				"Duplicate struct/union member '%s'",
				afield->name);
			return NULL;
		}
	}
	struct struct_field *field = xcalloc(1, sizeof(struct struct_field));

	if (afield->name && !size_only) {
		field->name = xstrdup(afield->name);
	}
	struct dimensions dim = {0};
	if (size_only) {
		dim = lookup_atype_with_dimensions(ctx, NULL, afield->type);
	} else {
		dim = lookup_atype_with_dimensions(ctx, &field->type, afield->type);
	}
	if (afield->next != NULL && dim.size == SIZE_UNDEFINED) {
		error(ctx, afield->type->loc, NULL,
			"Type of undefined size is not a valid struct/union member");
		return NULL;
	}
	if (dim.align == ALIGN_UNDEFINED) {
		error(ctx, afield->type->loc, NULL,
			"Type of undefined alignment is not a valid struct/union member");
		return NULL;
	}

	if (afield->offset) {
		type->struct_union.c_compat = false;
		struct expression in, out;
		check_expression(ctx, afield->offset, &in, NULL);
		field->offset = *offset;
		if (!eval_expr(ctx, &in, &out)) {
			error(ctx, in.loc, NULL,
				"Cannot evaluate field offset at compile time");
		} else if (!type_is_integer(ctx, out.result)) {
			error(ctx, in.loc, NULL,
				"Field offset must be an integer");
		} else if (type_is_signed(ctx, out.result) && out.literal.ival < 0) {
			error(ctx, in.loc, NULL,
				"Field offset must not be less than 0");
		} else if (out.literal.uval < *offset) {
			error(ctx, in.loc, NULL,
				"Field offset must be greater than or equal to previous field's offset");
		} else if (out.literal.uval < type->size) {
			error(ctx, in.loc, NULL,
				"Fields must not have overlapping storage");
		} else {
			field->offset = *offset = (size_t)out.literal.uval;
		}
	} else if (type->struct_union.packed) {
		field->offset = *offset = type->size;
	} else {
		*offset = type->size;
		if (dim.align != 0 && *offset % dim.align) {
			*offset += dim.align - (*offset % dim.align);
		}
		field->offset = *offset;
		assert(dim.align == 0 || field->offset % dim.align == 0);
	}

	if (dim.size == SIZE_UNDEFINED || type->size == SIZE_UNDEFINED) {
		type->size = SIZE_UNDEFINED;
	} else if (type->storage == STORAGE_STRUCT) {
		type->size = field->offset + dim.size;
	} else {
		*usize = dim.size > *usize ? dim.size : *usize;
	}
	type->align = dim.align > type->align ? dim.align : type->align;
	field->size = dim.size;
	return field;
}

static const struct type *type_store_lookup_type(struct context *ctx, const struct type *type);

bool
check_embedded_member(struct context *ctx,
	const struct ast_struct_union_field *afield,
	struct struct_field *member,
	const struct struct_field *fields)
{
	assert(member->type != NULL);
	const struct type *dealiased = type_dealias(ctx, member->type);
	if (dealiased->storage != STORAGE_STRUCT
			&& dealiased->storage != STORAGE_UNION) {
		error(ctx, afield->type->loc, NULL,
			"Cannot embed non-struct non-union alias");
		member->type = &builtin_type_error;
		return false;
	}

	for (struct struct_field *field = dealiased->struct_union.fields;
			field; field = field->next) {
		if (field->name != NULL) {
			if (struct_union_has_field(ctx, field->name, fields)) {
				// XXX: the location could be better
				error(ctx, afield->type->loc, NULL,
					"Duplicate struct/union member '%s'",
					field->name);
				return false;
			}
		} else {
			if (!check_embedded_member(ctx, afield, field, fields)) {
				return false;
			}
		}
	}

	return true;
}

void
shift_fields(struct context *ctx,
	const struct ast_struct_union_field *afield, struct struct_field *parent)
{
	if (parent->offset == 0) {
		// We need to return early here in order to avoid dealiasing an
		// embedded alias. This is acceptable at nonzero offsets, but we
		// need to keep the alias if it's at offset 0 because of
		// subtyping.
		return;
	}
	const struct type *type = type_dealias(ctx, parent->type);
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
			shift_fields(ctx, NULL, new);
		}
		// Sub-subfields are shifted by field->offset in the recursive
		// shift_fields call, delay adding it to new->offset to avoid
		// shifting by field->offset twice
		new->offset += field->offset;
	}

	parent->type = type_store_lookup_type(ctx, &new);
}

static void
struct_init_from_atype(struct context *ctx, struct type *type,
	const struct ast_struct_union_type *atype, bool size_only)
{
	// TODO: fields with size SIZE_UNDEFINED
	size_t usize = 0;
	size_t offset = 0;
	assert(type->storage == STORAGE_STRUCT || type->storage == STORAGE_UNION);
	struct struct_field **next = &type->struct_union.fields;
	type->struct_union.packed = atype->packed;
	for (const struct ast_struct_union_field *afield = &atype->fields;
			afield; afield = afield->next) {
		struct struct_field *field = struct_new_field(ctx, type,
			afield, &usize, &offset, size_only);
	if (field == NULL) {
			return;
		}
		if (size_only) {
			free(field);
			continue;
		} else if (!field->name) {
			if (!check_embedded_member(ctx, afield, field,
						type->struct_union.fields)) {
				return;
			}
			// We need to shift the embedded struct/union's fields
			// so that their offsets are from the start of the
			// parent type. This is a bit of a hack, but it makes
			// type_get_field far easier to implement and doesn't
			// cause any trouble in gen since offsets are only used
			// there for sorting fields.
			shift_fields(ctx, afield, field);
		}
		*next = field;
		next = &field->next;
	}

	if (type->storage == STORAGE_UNION) {
		type->size = usize;
	}
}

static bool
enforce_tagged_invariants(struct context *ctx, struct location loc,
		const struct type *type)
{
	int i;
	const struct type_tagged_union *tu;
	for (i = 0, tu = &type->tagged; tu; i++, tu = tu->next) {
		if (tu->type->storage == STORAGE_NULL) {
			error(ctx, loc, NULL,
				"Null type not allowed in this context");
			return false;
		}
		if (tu->type->size == SIZE_UNDEFINED) {
			error(ctx, loc, NULL,
				"Type of undefined size is not a valid tagged union member");
			return false;
		}
		assert(tu->type->align != ALIGN_UNDEFINED);
	}
	if (i <= 1) {
		error(ctx, loc, NULL,
			"Tagged unions must have at least two distinct members");
		return false;
	}
	return true;
}

static size_t
sum_tagged_memb(struct context *ctx,
		const struct type_tagged_union *u)
{
	size_t nmemb = 0;
	for (; u; u = u->next) {
		const struct type *type = u->type;
		if (type->storage == STORAGE_TAGGED) {
			nmemb += sum_tagged_memb(ctx, &type->tagged);
		} else {
			++nmemb;
		}
	}
	return nmemb;
}

// get next member of an incomplete tagged union without completing it
static void
tagged_or_atagged_member(struct context *ctx,
		const struct ast_type **atype, const struct type **type)
{
	const struct ast_type *_atype = *atype;
	while (_atype->storage == STORAGE_ALIAS && _atype->unwrap) {
		const struct scope_object *obj = scope_lookup(
			ctx->scope, &_atype->alias);
		if (!obj) {
			char *ident = identifier_unparse(&_atype->alias);
			error(ctx, _atype->loc, NULL,
				"Unknown object '%s'", ident);
			free(ident);
			*type = &builtin_type_error;
			return;
		}
		if (obj->otype != O_SCAN) {
			if (obj->otype == O_TYPE) {
				*type = type_dealias(ctx, obj->type);
				return;
			} else {
				char *ident = identifier_unparse(&obj->ident);
				error(ctx, _atype->loc, NULL,
					"Object '%s' is not a type", ident);
				free(ident);
				*type = &builtin_type_error;
				return;
			}
		}
		struct incomplete_declaration *idecl =
			(struct incomplete_declaration *)obj;
		if (idecl->type != IDECL_DECL
				|| idecl->decl.decl_type != ADECL_TYPE) {
			char *ident = identifier_unparse(&obj->ident);
			error(ctx, _atype->loc, NULL,
				"Object '%s' is not a type", ident);
			free(ident);
			*type = &builtin_type_error;
			return;
		}
		_atype = idecl->decl.type.type;
	}
	*type = NULL;
	*atype = _atype;
}

static size_t
sum_atagged_memb(struct context *ctx, const struct ast_tagged_union_type *u)
{
	size_t nmemb = 0;
	for (; u; u = u->next) {
		const struct type *type = NULL;
		const struct ast_type *atype = u->type;
		tagged_or_atagged_member(ctx, &atype, &type);
		if (type != NULL && type->storage == STORAGE_TAGGED) {
			nmemb += sum_tagged_memb(ctx, &type->tagged);
		} else if (atype->storage == STORAGE_TAGGED) {
			nmemb += sum_atagged_memb(ctx, &atype->tagged);
		} else {
			++nmemb;
		}
	}
	return nmemb;
}

static void
collect_tagged_memb(struct context *ctx,
		struct type_tagged_union **ta,
		const struct type_tagged_union *src,
		size_t *i)
{
	for (; src; src = src->next) {
		const struct type *type = src->type;
		if (type->storage == STORAGE_TAGGED) {
			collect_tagged_memb(ctx, ta, &type->tagged, i);
			continue;
		}
		struct type_tagged_union *tu;
		ta[*i] = tu = xcalloc(1, sizeof(struct type_tagged_union));
		tu->type = lower_flexible(ctx, type, NULL);
		*i += 1;
	}
}

static void
collect_atagged_memb(struct context *ctx,
		struct type_tagged_union **ta,
		const struct ast_tagged_union_type *atu,
		size_t *i)
{
	for (; atu; atu = atu->next) {
		const struct type *type = lookup_atype(ctx, atu->type);
		if (type->storage == STORAGE_TAGGED) {
			collect_tagged_memb(ctx, ta, &type->tagged, i);
			continue;
		}
		struct type_tagged_union *tu;
		ta[*i] = tu = xcalloc(1, sizeof(struct type_tagged_union));
		tu->type = lower_flexible(ctx, type, NULL);
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
tagged_init_from_atype(struct context *ctx,
	struct type *type, const struct ast_type *atype)
{
	size_t nmemb = sum_atagged_memb(ctx, &atype->tagged);
	struct type_tagged_union **tu =
		xcalloc(nmemb, sizeof(struct type_tagged_union *));
	size_t i = 0;
	collect_atagged_memb(ctx, tu, &atype->tagged, &i);
	tagged_init(type, tu, i);
	if (!enforce_tagged_invariants(ctx, atype->loc, type)) {
		*type = builtin_type_error;
	}
}

static struct dimensions
_tagged_size(struct context *ctx, const struct ast_tagged_union_type *u)
{
	struct dimensions dim = {0};
	for (; u; u = u->next) {
		struct dimensions memb = {0};
		const struct type *type = NULL;
		const struct ast_type *atype = u->type;
		tagged_or_atagged_member(ctx, &atype, &type);
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
			memb = _tagged_size(ctx, &atype->tagged);
		} else {
			memb = lookup_atype_with_dimensions(ctx, NULL, atype);
		}
		if (memb.size == SIZE_UNDEFINED) {
			error(ctx, atype->loc, NULL,
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
tagged_size(struct context *ctx, const struct ast_type *atype)
{
	struct dimensions dim = _tagged_size(ctx, &atype->tagged);
	if (dim.align < builtin_type_u32.align) {
		dim.align = builtin_type_u32.align;
	}
	dim.size += builtin_type_u32.size % dim.align + builtin_type_u32.align;
	return dim;
}


static struct dimensions
tuple_init_from_atype(struct context *ctx,
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
			memb = lookup_atype_with_dimensions(ctx, &cur->type, atuple->type);
		} else {
			memb = lookup_atype_with_dimensions(ctx, NULL, atuple->type);
		}
		if (memb.size == SIZE_UNDEFINED) {
			error(ctx, atype->loc, NULL,
				"Type of undefined size is not a valid tuple member");
			if (type) {
				*type = builtin_type_error;
			}
			return (struct dimensions){0};
		}
		if (memb.align != 0) {
			offset = dim.size;
			if (dim.size % memb.align) {
				offset += memb.align - dim.size % memb.align;
			}
			dim.size = offset + memb.size;
		}
		if (dim.align < memb.align) {
			dim.align = memb.align;
		}

		atuple = atuple->next;
		if (type) {
			if (memb.align != 0) {
				cur->offset = offset;
			}
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

static void
add_padding(size_t *size, size_t align)
{
	if (*size != SIZE_UNDEFINED && *size != 0 && *size % align != 0) {
		*size += align - *size % align;
	}
}

static bool
default_param_from_atype(struct context *ctx,
	const struct ast_function_parameters *aparam,
	struct type_func_param *param)
{
	// This is leaked. check_expression makes a flexible ref that may be
	// updated later, so it cannot be on the stack.
	struct expression *in = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aparam->default_value, in, param->type);
	if (!type_is_assignable(ctx, param->type, in->result)) {
		char *restypename = gen_typename(in->result);
		char *partypename = gen_typename(param->type);
		error(ctx, aparam->loc, NULL,
			"Result value %s is not assignable to parameter type %s",
			restypename, partypename);
		free(restypename);
		free(partypename);
		return false;
	}
	param->default_value = xcalloc(1, sizeof(struct expression));
	struct expression *cast = lower_implicit_cast(ctx, param->type, in);
	if (!eval_expr(ctx, cast, param->default_value)) {
		error(ctx, aparam->loc, NULL,
			"Unable to evaluate default parameter at compile time");
		return false;
	}
	// TODO remove this check once it works
	if (param->default_value->result->storage == STORAGE_POINTER &&
			param->default_value->literal.object != NULL) {
		error(ctx, aparam->loc, NULL,
			"Non-null pointer optional parameters are not currently supported. Will fix.");
		return false;
	}
	return true;
}

static struct dimensions
type_init_from_atype(struct context *ctx,
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

	struct scope_object *obj = NULL;
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
	case STORAGE_DONE:
		builtin = builtin_type_for_storage(type->storage, false);
		type->size = builtin->size;
		type->align = builtin->align;
		break;
	case STORAGE_ALIAS:
		obj = scope_lookup(ctx->scope, &atype->alias);
		if (!obj) {
			char *ident = identifier_unparse(&atype->alias);
			error(ctx, atype->loc, NULL,
				"Unresolvable identifier '%s'", ident);
			free(ident);
			*type = builtin_type_error;
			return (struct dimensions){0};
		}

		if (obj->otype == O_SCAN) {
			// an incomplete declaration was encountered
			struct incomplete_declaration *idecl =
				(struct incomplete_declaration *)obj;
			if (size_only && idecl->type == IDECL_DECL) {
				wrap_resolver(ctx, obj, resolve_dimensions);
				type->size = obj->type->size;
				type->align = obj->type->align;
				break;
			}
			// complete it first and then proceed normally
			wrap_resolver(ctx, obj, resolve_type);
		}

		if (obj->otype != O_TYPE) {
			char *ident = identifier_unparse(&obj->ident);
			error(ctx, atype->loc, NULL,
				"Object '%s' is not a type", ident);
			free(ident);
			*type = builtin_type_error;
			return (struct dimensions){0};
		}

		type->storage = obj->type->storage;
		if (obj->type->storage == STORAGE_ENUM) {
			type->_enum = obj->type->_enum;
		} else if (atype->unwrap) {
			*type = *type_dealias(ctx, obj->type);
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
		type->array.length = ast_array_len(ctx, atype);
		struct dimensions memb = {0};
		if (size_only) {
			memb = lookup_atype_with_dimensions(ctx,
				NULL, atype->array.members);
		} else {
			memb = lookup_atype_with_dimensions(ctx,
				&type->array.members, atype->array.members);
			if (type->array.members->storage == STORAGE_ERROR) {
				*type = builtin_type_error;
				return (struct dimensions){0};
			}
		}
		if (memb.size == 0) {
			error(ctx, atype->loc, NULL,
				"Type of size 0 is not a valid array member");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		if (memb.size == SIZE_UNDEFINED) {
			error(ctx, atype->loc, NULL,
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
		type->func.result = lookup_atype(ctx, atype->func.result);
		type->func.variadism = atype->func.variadism;
		struct type_func_param *param, **next = &type->func.params;
		bool has_optional = false;
		for (struct ast_function_parameters *aparam = atype->func.params;
				aparam; aparam = aparam->next) {
			param = *next = xcalloc(1, sizeof(struct type_func_param));
			param->type = lookup_atype(ctx, aparam->type);
			if (aparam->default_value != NULL) {
				has_optional = true;
				if (!default_param_from_atype(ctx,
						aparam, param)) {
					*type = builtin_type_error;
					return (struct dimensions){0};
				}
			} else if (has_optional) {
				error(ctx, atype->loc, NULL,
					"Required function parameter may not follow optional parameters");
				*type = builtin_type_error;
				return (struct dimensions){0};
			}
			if (param->type->size == SIZE_UNDEFINED) {
				error(ctx, atype->loc, NULL,
					"Function parameter types must have defined size");
				*type = builtin_type_error;
				return (struct dimensions){0};
			}
			if (atype->func.variadism == VARIADISM_HARE
					&& !aparam->next) {
				param->type = type_store_lookup_slice(
					ctx, aparam->loc, param->type);
			}
			next = &param->next;
		}
		if (atype->func.variadism != VARIADISM_NONE && has_optional) {
			error(ctx, atype->loc, NULL,
				"Variadic function may not have optional parameters");
			*type = builtin_type_error;
			return (struct dimensions){0};
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
			ctx, atype->pointer.referent);
		if (type->pointer.referent->storage == STORAGE_ERROR) {
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		if (type->pointer.referent->size == 0) {
			error(ctx, atype->loc, NULL,
				"Can't have pointer to zero-sized type");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		if (type->pointer.referent->storage == STORAGE_NEVER) {
			error(ctx, atype->loc, NULL,
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
		type->array.members = lookup_atype(ctx, atype->array.members);
		if (type->array.members->storage == STORAGE_ERROR) {
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		if (type->array.members->size == 0) {
			error(ctx, atype->loc, NULL,
				"Type of size 0 is not a valid slice member");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		if (type->array.members->storage == STORAGE_NEVER) {
			error(ctx, atype->loc, NULL,
				"Never is not a valid slice member");
			*type = builtin_type_error;
			return (struct dimensions){0};
		}
		type->array.length = SIZE_UNDEFINED;
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		type->struct_union.c_compat = !atype->struct_union.packed;
		type->struct_union.packed = atype->struct_union.packed;
		struct_init_from_atype(ctx, type, &atype->struct_union, size_only);
		break;
	case STORAGE_TAGGED:
		if (size_only) {
			struct dimensions tagged = tagged_size(ctx, atype);
			type->size = tagged.size;
			type->align = tagged.align;
		} else {
			tagged_init_from_atype(ctx, type, atype);
		}
		break;
	case STORAGE_TUPLE:
		if (size_only) {
			struct dimensions tup;
			tup = tuple_init_from_atype(ctx, NULL, atype);
			type->size = tup.size;
			type->align = tup.align;
		} else {
			tuple_init_from_atype(ctx, type, atype);
		}
		break;
	}

	struct dimensions dim = {
		.size = type->size,
		.align = type->align,
	};
	if (type->storage != STORAGE_STRUCT || !type->struct_union.packed) {
		// padding an alias can only break packed structs
		if (type->storage != STORAGE_ALIAS) {
			add_padding(&dim.size, dim.align);
		}
	}
	return dim;
}

static const struct type *
_type_store_lookup_type(
	struct context *ctx,
	const struct type *type,
	const struct dimensions *dims)
{
	const struct type *builtin = builtin_for_type(type);
	if (builtin) {
		return builtin;
	}

	uint32_t hash = type_hash(type);
	struct type_bucket **next = &(*ctx->store)[hash % TYPE_STORE_BUCKETS],
		*bucket = NULL;

	while (*next) {
		bucket = *next;
		if (bucket->type.id == hash) {
			if (bucket->type.storage == STORAGE_ALIAS) {
				type = type->alias.type;
				bucket->type.alias.type = type;
				if (type && type->storage == STORAGE_ERROR) {
					return &builtin_type_error;
				}
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
type_store_lookup_type(struct context *ctx, const struct type *type)
{
	if (type->storage != STORAGE_ALIAS) {
		return _type_store_lookup_type(ctx, type, NULL);
	}
	// References to type aliases always inherit the flags that the
	// alias was defined with
	struct type psuedotype = *type;
	const struct scope_object *obj = scope_lookup(
		ctx->scope, &type->alias.name);
	psuedotype.flags |= obj->type->flags;
	return type_store_lookup_alias(ctx, &psuedotype, NULL);
}

static struct dimensions
lookup_atype_with_dimensions(struct context *ctx,
	const struct type **type,
	const struct ast_type *atype)
{
	struct type temp = {0};
	struct dimensions dim = {0};
	if (type) {
		dim = type_init_from_atype(ctx, &temp, atype);
		*type = type_store_lookup_type(ctx, &temp);
	} else {
		dim = type_init_from_atype(ctx, NULL, atype);
	}
	return dim;
}

static const struct type *
lookup_atype(struct context *ctx, const struct ast_type *atype)
{
	const struct type *type = NULL;
	lookup_atype_with_dimensions(ctx, &type, atype);
	return type;
}

const struct type *
type_store_lookup_atype(struct context *ctx, const struct ast_type *atype)
{
	if (atype->storage == STORAGE_NULL) {
		return &builtin_type_null;
	}
	return lookup_atype(ctx, atype);
}

// Compute dimensions of an incomplete type without completing it
struct dimensions
type_store_lookup_dimensions(struct context *ctx, const struct ast_type *atype)
{
	return type_init_from_atype(ctx, NULL, atype);
}

const struct type *
type_store_lookup_with_flags(struct context *ctx,
	const struct type *type, unsigned int flags)
{
	if (type->flags == flags) {
		return type;
	}
	struct type new = *type;
	new.flags = flags;
	return _type_store_lookup_type(ctx, &new, NULL);
}

const struct type *
type_store_lookup_pointer(struct context *ctx, struct location loc,
	const struct type *referent, unsigned int ptrflags)
{
	if (referent->storage == STORAGE_ERROR) {
		return &builtin_type_error;
	}
	if (referent->storage == STORAGE_NULL) {
		error(ctx, loc, NULL, "Null type not allowed in this context");
		return &builtin_type_error;
	}
	if (referent->size == 0) {
		error(ctx, loc, NULL, "Can't have pointer to zero-sized type");
		return &builtin_type_error;
	}
	if (referent->storage == STORAGE_NEVER) {
		error(ctx, loc, NULL, "Can't have pointer to never");
		return &builtin_type_error;
	}
	referent = lower_flexible(ctx, referent, NULL);

	struct type ptr = {
		.storage = STORAGE_POINTER,
		.pointer = {
			.referent = referent,
			.flags = ptrflags,
		},
		.size = builtin_type_uintptr.size,
		.align = builtin_type_uintptr.align,
	};
	return type_store_lookup_type(ctx, &ptr);
}

const struct type *
type_store_lookup_array(struct context *ctx, struct location loc,
	const struct type *members, size_t len, bool expandable)
{
	if (members->storage == STORAGE_ERROR) {
		return &builtin_type_error;
	}
	if (members->storage == STORAGE_NULL) {
		error(ctx, loc, NULL, "Null type not allowed in this context");
		return &builtin_type_error;
	}
	members = lower_flexible(ctx, members, NULL);
	if (members->size == 0) {
		error(ctx, loc, NULL,
			"Type of size 0 is not a valid array member");
		return &builtin_type_error;
	}
	if (members->size == SIZE_UNDEFINED) {
		error(ctx, loc, NULL,
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
	return type_store_lookup_type(ctx, &array);
}

const struct type *
type_store_lookup_slice(struct context *ctx, struct location loc,
	const struct type *members)
{
	if (members->storage == STORAGE_ERROR) {
		return &builtin_type_error;
	}
	if (members->storage == STORAGE_NULL) {
		error(ctx, loc, NULL, "Null type not allowed in this context");
		return &builtin_type_error;
	}
	members = lower_flexible(ctx, members, NULL);
	if (members->size == 0) {
		error(ctx, loc, NULL,
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
	return type_store_lookup_type(ctx, &slice);
}

const struct type *
type_store_lookup_alias(struct context *ctx,
		const struct type *type,
		const struct dimensions *dims)
{
	return _type_store_lookup_type(ctx, type, dims);
}


// Sorts members by id and deduplicates entries. Does not enforce usual tagged
// union invariants. The returned type is not a singleton.
static const struct type *
lookup_tagged(struct context *ctx, struct type_tagged_union *tags)
{
	struct type type = {
		.storage = STORAGE_TAGGED,
	};
	size_t nmemb = sum_tagged_memb(ctx, tags);
	struct type_tagged_union **tu =
		xcalloc(nmemb, sizeof(struct type_tagged_union *));
	size_t i = 0;
	collect_tagged_memb(ctx, tu, tags, &i);
	tagged_init(&type, tu, nmemb);
	struct type *ret = xcalloc(1, sizeof(struct type));
	*ret = type;
	return ret;
}

const struct type *
type_store_lookup_tagged(struct context *ctx, struct location loc,
		struct type_tagged_union *tags)
{
	const struct type *type = lookup_tagged(ctx, tags);
	if (!enforce_tagged_invariants(ctx, loc, type)) {
		return &builtin_type_error;
	}
	return type_store_lookup_type(ctx, type);
}

const struct type *
type_store_tagged_to_union(struct context *ctx, const struct type *tagged)
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
	return _type_store_lookup_type(ctx, &type, NULL);
}

const struct type *
type_store_lookup_tuple(struct context *ctx, struct location loc,
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
			error(ctx, loc, NULL,
				"Null type not allowed in this context");
			return &builtin_type_error;
		}
		t->type = lower_flexible(ctx, t->type, NULL);
		if (t->type->size == SIZE_UNDEFINED) {
			error(ctx, loc, NULL,
				"Type of undefined size is not a valid tuple member");
			return &builtin_type_error;
		}
		assert(t->type->align != ALIGN_UNDEFINED);

		if (t->type->align > type.align) {
			type.align = t->type->align;
		}
		if (t->type->align != 0) {
			t->offset = type.size;
			if (type.size % t->type->align) {
				t->offset += t->type->align - type.size % t->type->align;
			}
			type.size = t->offset + t->type->size;
		}
	}
	type.tuple = *values;
	return type_store_lookup_type(ctx, &type);
}

const struct type *
type_store_lookup_enum(struct context *ctx, const struct ast_type *atype,
	bool exported)
{
	struct type type = {0};
	type.storage = STORAGE_ENUM;
	type.flags = atype->flags;
	mkident(ctx, &type.alias.ident, &atype->alias, NULL);
	identifier_dup(&type.alias.name, &atype->alias);
	type.alias.exported = exported;
	type.alias.type =
		builtin_type_for_storage(atype->_enum.storage, false);
	if (!type_is_integer(ctx, type.alias.type)
			&& type.alias.type->storage != STORAGE_RUNE) {
		error(ctx, atype->loc, NULL,
			"Enum storage must be an integer or rune");
		return &builtin_type_error;
	}
	type.size = type.alias.type->size;
	type.align = type.alias.type->size;
	return type_store_lookup_type(ctx, &type);
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
type_store_reduce_result(struct context *ctx, struct location loc,
		struct type_tagged_union *in)
{
	if (!in) {
		return &builtin_type_never;
	} else if (!in->next) {
		return in->type;
	}

	const struct type *type = lookup_tagged(ctx, in);
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
				jt = type_store_lookup_with_flags(ctx, jt,
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
				it = type_store_lookup_pointer(ctx, loc,
					it->pointer.referent, PTR_NULLABLE);
				jt = type_store_lookup_pointer(ctx, loc,
					jt->pointer.referent, PTR_NULLABLE);
				if (it == jt) {
					dropped = true;
					*tu = i->next;
					j->type = jt;
					break;
				}
			}
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
		ptr->type = type_store_lookup_pointer(ctx, loc,
			ptr->type->pointer.referent, PTR_NULLABLE);
	}

	if (in == NULL) {
		return &builtin_type_never;
	} else if (in->next == NULL) {
		return in->type;
	}
	return type_store_lookup_tagged(ctx, loc, in);
}
