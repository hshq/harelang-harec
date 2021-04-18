#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "eval.h"
#include "expr.h"
#include "scope.h"
#include "type_store.h"
#include "types.h"
#include "util.h"

static enum eval_result
eval_access(struct context *ctx, struct expression *in, struct expression *out)
{
	out->type = EXPR_CONSTANT;
	out->result = in->result;

	// TODO: Probably have not considered all of the edge cases here
	switch (in->access.type) {
	case ACCESS_IDENTIFIER:
		out->constant.object = in->access.object;
		out->constant.ival = 0;
		assert(in->access.object->otype == O_DECL); // TODO: Bubble this up
		break;
	case ACCESS_INDEX:
		assert(0); // TODO
	case ACCESS_FIELD:
		assert(0); // TODO
	case ACCESS_TUPLE:
		out->type = EXPR_CONSTANT;
		struct expression tmp = {0};
		enum eval_result r = eval_expr(ctx, in->access.value, &tmp);
		if (r != EVAL_OK) {
			return r;
		}
		size_t i = tmp.constant.uval;
		// ensure the entire tuple is known at compile time
		r = eval_expr(ctx, in->access.tuple, &tmp);
		if (r != EVAL_OK) {
			return r;
		}
		const struct tuple_constant *tuple = tmp.constant.tuple;
		for (; i > 0; --i) {
			tuple = tuple->next;
		}
		return eval_expr(ctx, tuple->value, out);
	}

	return EVAL_OK;
}

static uintmax_t
itrunc(const struct type *type, uintmax_t val)
{
	switch (type->storage) {
	case STORAGE_U8:
		return (uint8_t)val;
	case STORAGE_U16:
		return (uint16_t)val;
	case STORAGE_U32:
	case STORAGE_RUNE:
		return (uint32_t)val;
	case STORAGE_U64:
		return (uint64_t)val;
	case STORAGE_I8:
		return (int8_t)((val >> 24) | (val & 0x7F));
	case STORAGE_I16:
		return (int16_t)((val >> 16) | (val & 0x7FF));
	case STORAGE_I32:
		return (int32_t)((val >> 8) | (val & 0x7FFFF));
	case STORAGE_I64:
		return (int64_t)val;
	case STORAGE_INT:
		return (int)val;
	case STORAGE_UINT:
		return (unsigned int)val;
	case STORAGE_ARRAY:
	case STORAGE_ICONST:
	case STORAGE_POINTER:
	case STORAGE_SIZE:
	case STORAGE_UINTPTR:
		return val;
	case STORAGE_BOOL:
		return (bool)val;
	case STORAGE_NULL:
		return (uintptr_t)NULL;
	case STORAGE_ALIAS:
		return itrunc(type_dealias(type), val);
	case STORAGE_ENUM:
		return itrunc(builtin_type_for_storage(type->_enum.storage, false), val);
	case STORAGE_CHAR:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VOID:
		assert(0);
	}
	assert(0);
}

enum eval_result
eval_binarithm(struct context *ctx, struct expression *in, struct expression *out)
{
	struct expression lvalue = {0}, rvalue = {0};
	enum eval_result r = eval_expr(ctx, in->binarithm.lvalue, &lvalue);
	if (r != EVAL_OK) {
		return r;
	}
	r = eval_expr(ctx, in->binarithm.rvalue, &rvalue);
	if (r != EVAL_OK) {
		return r;
	}

	bool blval = lvalue.constant.bval, brval = rvalue.constant.bval, bval;
	intmax_t ilval = itrunc(lvalue.result, lvalue.constant.ival),
		 irval = itrunc(rvalue.result, rvalue.constant.ival), ival;
	uintmax_t ulval = itrunc(lvalue.result, lvalue.constant.uval),
		  urval = itrunc(rvalue.result, rvalue.constant.uval), uval;
	// Type promotion is lowered in check
	assert(lvalue.result->storage == rvalue.result->storage);
	switch (in->binarithm.op) {
	case BIN_BAND:
		if (type_is_signed(lvalue.result)) {
			ival = ilval & irval;
		} else {
			uval = ulval & urval;
		}
		break;
	case BIN_BOR:
		if (type_is_signed(lvalue.result)) {
			ival = ilval | irval;
		} else {
			uval = ulval | urval;
		}
		break;
	case BIN_DIV:
		if (type_is_signed(lvalue.result)) {
			ival = ilval / irval;
		} else {
			uval = ulval / urval;
		}
		break;
	case BIN_LSHIFT:
		assert(!type_is_signed(rvalue.result));
		uval = ulval << urval;
		break;
	case BIN_MINUS:
		if (type_is_signed(lvalue.result)) {
			ival = ilval - irval;
		} else {
			uval = ulval - urval;
		}
		break;
	case BIN_MODULO:
		if (type_is_signed(lvalue.result)) {
			ival = ilval % irval;
		} else {
			uval = ulval % urval;
		}
		break;
	case BIN_PLUS:
		if (type_is_signed(lvalue.result)) {
			ival = ilval + irval;
		} else {
			uval = ulval + urval;
		}
		break;
	case BIN_RSHIFT:
		assert(!type_is_signed(rvalue.result));
		uval = ulval >> urval;
		break;
	case BIN_TIMES:
		if (type_is_signed(lvalue.result)) {
			ival = ilval * irval;
		} else {
			uval = ulval * urval;
		}
		break;
	case BIN_BXOR:
		if (type_is_signed(lvalue.result)) {
			ival = ilval ^ irval;
		} else {
			uval = ulval ^ urval;
		}
		break;
	// Logical arithmetic
	case BIN_GREATER:
		if (type_is_signed(lvalue.result)) {
			bval = ilval > irval;
		} else {
			bval = ulval > urval;
		}
		break;
	case BIN_GREATEREQ:
		if (type_is_signed(lvalue.result)) {
			bval = ilval >= irval;
		} else {
			bval = ulval >= urval;
		}
		break;
	case BIN_LAND:
		assert(type_dealias(lvalue.result)->storage == STORAGE_BOOL
			&& type_dealias(rvalue.result)->storage == STORAGE_BOOL);
		bval = blval && brval;
		break;
	case BIN_LEQUAL:
		if (type_is_signed(lvalue.result)) {
			bval = ilval == irval;
		} else {
			bval = ulval == urval;
		}
		break;
	case BIN_LESS:
		if (type_is_signed(lvalue.result)) {
			bval = ilval < irval;
		} else {
			bval = ulval < urval;
		}
		break;
	case BIN_LESSEQ:
		if (type_is_signed(lvalue.result)) {
			bval = ilval <= irval;
		} else {
			bval = ulval <= urval;
		}
		break;
	case BIN_LOR:
		assert(type_dealias(lvalue.result)->storage == STORAGE_BOOL
			&& type_dealias(rvalue.result)->storage == STORAGE_BOOL);
		bval = blval || brval;
		break;
	case BIN_LXOR:
		assert(type_dealias(lvalue.result)->storage == STORAGE_BOOL
			&& type_dealias(rvalue.result)->storage == STORAGE_BOOL);
		bval = blval != brval;
		break;
	case BIN_NEQUAL:
		if (type_is_signed(lvalue.result)) {
			bval = ilval != irval;
		} else {
			bval = ulval != urval;
		}
		break;
	}
	uval = itrunc(in->result, uval);
	ival = itrunc(in->result, ival);
	out->type = EXPR_CONSTANT;
	out->result = in->result;
	if (type_is_signed(in->result)) {
		out->constant.ival = ival;
	} else if (type_dealias(in->result)->storage == STORAGE_BOOL) {
		out->constant.bval = bval;
	} else {
		out->constant.uval = uval;
	}
	return EVAL_OK;
}

enum eval_result
eval_const(struct context *ctx, struct expression *in, struct expression *out)
{
	out->type = EXPR_CONSTANT;
	out->result = in->result;
	enum type_storage storage = type_dealias(out->result)->storage;
	if (storage == STORAGE_ENUM) {
		storage = type_dealias(out->result)->_enum.storage;
	}
	struct array_constant **next;
	switch (storage) {
	case STORAGE_ALIAS:
	case STORAGE_ENUM:
		assert(0); // Handled above
	case STORAGE_ARRAY:
		next = &out->constant.array;
		for (struct array_constant *arr = in->constant.array; arr;
				arr = arr->next) {
			struct array_constant *aconst = *next =
				xcalloc(sizeof(struct array_constant), 1);
			aconst->expand = arr->expand;
			aconst->value = xcalloc(sizeof(struct expression), 1);
			eval_expr(ctx, arr->value, aconst->value);
			next = &aconst->next;
		}
		break;
	case STORAGE_FUNCTION:
	case STORAGE_SLICE:
		assert(0); // TODO
	case STORAGE_STRING:
		out->constant.string.len = in->constant.string.len;
		out->constant.string.value = xcalloc(1, in->constant.string.len);
		memcpy(out->constant.string.value,
			in->constant.string.value,
			in->constant.string.len);
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		assert(0); // TODO
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_NULL:
	case STORAGE_POINTER:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VOID:
		out->constant = in->constant;
		break;
	}
	return EVAL_OK;
}

enum eval_result
eval_cast(struct context *ctx, struct expression *in, struct expression *out)
{
	struct expression val = {0};
	enum eval_result r = eval_expr(ctx, in->cast.value, &val);
	if (r != EVAL_OK) {
		return r;
	}

	const struct type *to = type_dealias(in->result),
	      *from = type_dealias(val.result);
	if (to->storage == from->storage) {
		*out = val;
		return EVAL_OK;
	}

	// XXX: We should also be able to handle expressions which use
	// symbols/identifiers
	out->type = EXPR_CONSTANT;
	out->result = to;

	switch (to->storage) {
	case STORAGE_POINTER:
		if (from->storage == STORAGE_NULL) {
			out->constant.uval = 0;
			return EVAL_OK;
		}
		assert(from->storage == STORAGE_POINTER
			|| from->storage == STORAGE_UINTPTR);
		out->constant.uval = val.constant.uval;
		return EVAL_OK;
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_SIZE:
	case STORAGE_RUNE:
		out->constant.uval = itrunc(to, val.constant.uval);
		return EVAL_OK;
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
		assert(val.result->storage == STORAGE_ARRAY);
		out->constant = val.constant;
		return EVAL_OK;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_CHAR:
	case STORAGE_ENUM:
	case STORAGE_NULL:
	case STORAGE_TAGGED:
		assert(0); // TODO
	case STORAGE_ALIAS:
		assert(0); // Handled above
	case STORAGE_BOOL:
	case STORAGE_FUNCTION:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
		assert(0); // Invariant
	case STORAGE_VOID:
		break; // no-op
	}

	assert(0); // Unreachable
}

enum eval_result
eval_measurement(struct context *ctx, struct expression *in, struct expression *out)
{
	assert(in->type == EXPR_MEASURE);
	out->type = EXPR_CONSTANT;
	switch (in->measure.op) {
	case M_LEN:
		assert(0); // TODO
	case M_SIZE:
		out->result = &builtin_type_size;
		out->constant.uval = in->measure.type->size;
		return EVAL_OK;
	case M_OFFSET:
		assert(0); // TODO
	}
	assert(0);
}

static void
constant_default(struct context *ctx, struct expression *v)
{
	struct expression b = {0};
	switch (type_dealias(v->result)->storage) {
	case STORAGE_POINTER:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_SIZE:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_CHAR:
	case STORAGE_ENUM:
	case STORAGE_NULL:
	case STORAGE_RUNE:
	case STORAGE_BOOL:
		break; // calloc does this for us
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		b.type = EXPR_STRUCT;
		b.result = v->result;
		b._struct.autofill = true;
		enum eval_result r = eval_expr(ctx, &b, v);
		assert(r == EVAL_OK);
		break;
	case STORAGE_STRING:
		v->constant.string.value = NULL;
		v->constant.string.len = 0;
		break;
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
		v->constant.array = xcalloc(1, sizeof(struct array_constant));
		v->constant.array->expand = true;
		v->constant.array->value = xcalloc(1, sizeof(struct expression));
		v->constant.array->value->type = EXPR_CONSTANT;
		v->constant.array->value->result =
			type_dealias(v->result)->array.members;
		constant_default(ctx, v->constant.array->value);
		break;
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		assert(0); // TODO
	case STORAGE_ALIAS:
	case STORAGE_FUNCTION:
		assert(0); // Invariant
	case STORAGE_VOID:
		break; // no-op
	}
}

static int
field_compar(const void *_a, const void *_b)
{
	const struct struct_constant **a = (const struct struct_constant **)_a;
	const struct struct_constant **b = (const struct struct_constant **)_b;
	return (*a)->field->offset - (*b)->field->offset;
}

enum eval_result
eval_struct(struct context *ctx, struct expression *in, struct expression *out)
{
	assert(in->type == EXPR_STRUCT);
	assert(type_dealias(in->result)->storage != STORAGE_UNION); // TODO
	const struct type *type = type_dealias(in->result);
	out->type = EXPR_CONSTANT;

	size_t n = 0;
	for (const struct struct_field *field = type->struct_union.fields;
			field; field = field->next) {
		++n;
	}
	assert(n > 0);

	size_t i = 0;
	struct struct_constant **fields =
		xcalloc(n, sizeof(struct struct_constant *));
	for (const struct struct_field *field = type->struct_union.fields;
			field; field = field->next, ++i) {
		const struct expr_struct_field *field_in = NULL;
		for (field_in = &in->_struct.fields; field_in; field_in = field_in->next) {
			if (field_in->field == field) {
				break;
			}
		}

		struct struct_constant *cfield = fields[i] =
			xcalloc(1, sizeof(struct struct_constant));
		cfield->field = field;
		cfield->value = xcalloc(1, sizeof(struct expression));

		if (!field_in) {
			assert(in->_struct.autofill);
			cfield->value->type = EXPR_CONSTANT;
			cfield->value->result = field->type;
			constant_default(ctx, cfield->value);
		} else {
			enum eval_result r = eval_expr(ctx,
				field_in->value, cfield->value);
			if (r != EVAL_OK) {
				return r;
			}
		}
	}

	qsort(fields, n, sizeof(struct struct_constant *), field_compar);

	for (size_t i = 0; i < n - 1; ++i) {
		fields[i]->next = fields[i + 1];
	}

	out->constant._struct = fields[0];
	out->result = in->result;
	free(fields);
	return EVAL_OK;
}

static enum eval_result
eval_tuple(struct context *ctx, struct expression *in, struct expression *out)
{
	assert(in->type == EXPR_TUPLE);
	const struct type *type = type_dealias(in->result);
	out->type = EXPR_CONSTANT;


	struct tuple_constant *out_tuple_start, *out_tuple;
	out_tuple_start = out_tuple = xcalloc(1, sizeof(struct tuple_constant));
	const struct expression_tuple *in_tuple = &in->tuple;
	for (const struct type_tuple *field_type = &type->tuple; field_type;
			field_type = field_type->next) {
		out_tuple->value = xcalloc(1, sizeof(struct expression));
		enum eval_result r =
			eval_expr(ctx, in_tuple->value, out_tuple->value);
		if (r != EVAL_OK) {
			return r;
		}
		out_tuple->field = field_type;
		if (in_tuple->next) {
			in_tuple = in_tuple->next;
			out_tuple->next =
				xcalloc(1, sizeof(struct tuple_constant));
			out_tuple = out_tuple->next;
		}
	}

	out->constant.tuple = out_tuple_start;
	out->result = in->result;
	return EVAL_OK;
}


static enum eval_result
eval_unarithm(struct context *ctx, struct expression *in, struct expression *out)
{
	struct expression lvalue = {0};
	enum eval_result r = eval_expr(ctx, in->binarithm.lvalue, &lvalue);
	if (r != EVAL_OK) {
		return r;
	}

	out->type = EXPR_CONSTANT;
	switch (in->unarithm.op) {
	case UN_ADDRESS:
		assert(lvalue.type == EXPR_CONSTANT);
		assert(lvalue.constant.object);
		out->result = type_store_lookup_pointer(
			ctx->store, lvalue.result, 0);
		out->constant = lvalue.constant;
		break;
	case UN_BNOT:
	case UN_DEREF:
	case UN_LNOT:
	case UN_MINUS:
	case UN_PLUS:
		assert(0); // TODO
	}

	return EVAL_OK;
}

enum eval_result
eval_expr(struct context *ctx, struct expression *in, struct expression *out)
{
	switch (in->type) {
	case EXPR_ACCESS:
		return eval_access(ctx, in, out);
	case EXPR_BINARITHM:
		return eval_binarithm(ctx, in, out);
	case EXPR_CAST:
		return eval_cast(ctx, in, out);
	case EXPR_CONSTANT:
		return eval_const(ctx, in, out);
	case EXPR_MEASURE:
		return eval_measurement(ctx, in, out);
	case EXPR_STRUCT:
		return eval_struct(ctx, in, out);
	case EXPR_SLICE:
		assert(0); // TODO
	case EXPR_TUPLE:
		return eval_tuple(ctx, in, out);
	case EXPR_UNARITHM:
		return eval_unarithm(ctx, in, out);
	case EXPR_ALLOC:
	case EXPR_APPEND:
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINDING:
	case EXPR_BREAK:
	case EXPR_CONTINUE:
	case EXPR_CALL:
	case EXPR_DEFER:
	case EXPR_DELETE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
	case EXPR_LIST:
	case EXPR_MATCH:
	case EXPR_PROPAGATE:
	case EXPR_RETURN:
	case EXPR_SWITCH:
		// Excluded from translation-compatible subset
		return EVAL_INVALID;
	}
	assert(0); // Unreachable
}
