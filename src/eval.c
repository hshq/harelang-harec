#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "eval.h"
#include "expr.h"
#include "scope.h"
#include "type_store.h"
#include "types.h"
#include "util.h"

static uintmax_t
itrunc(const struct type *type, uintmax_t val)
{
	switch (type->storage) {
	case TYPE_STORAGE_U8:
		return (uint8_t)val;
	case TYPE_STORAGE_U16:
		return (uint16_t)val;
	case TYPE_STORAGE_U32:
		return (uint32_t)val;
	case TYPE_STORAGE_U64:
		return (uint64_t)val;
	case TYPE_STORAGE_I8:
		return (int8_t)((val >> 24) | (val & 0x7F));
	case TYPE_STORAGE_I16:
		return (int16_t)((val >> 16) | (val & 0x7FF));
	case TYPE_STORAGE_I32:
		return (int32_t)((val >> 8) | (val & 0x7FFFF));
	case TYPE_STORAGE_I64:
		return (int64_t)val;
	case TYPE_STORAGE_INT:
		return (int)val;
	case TYPE_STORAGE_UINT:
		return (unsigned int)val;
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_UINTPTR:
		return val;
	case TYPE_STORAGE_BOOL:
		return (bool)val;
	case TYPE_STORAGE_NULL:
		return (uintptr_t)NULL;
	case TYPE_STORAGE_ALIAS:
		return itrunc(type_dealias(type), val);
	case TYPE_STORAGE_ENUM:
		return itrunc(builtin_type_for_storage(type->_enum.storage, false), val);
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_VOID:
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
	assert(lvalue.result->storage == rvalue.result->storage); // TODO: promotion
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
		assert(type_dealias(lvalue.result)->storage == TYPE_STORAGE_BOOL
			&& type_dealias(rvalue.result)->storage == TYPE_STORAGE_BOOL);
		bval = blval || brval;
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
		assert(type_dealias(lvalue.result)->storage == TYPE_STORAGE_BOOL
			&& type_dealias(rvalue.result)->storage == TYPE_STORAGE_BOOL);
		bval = blval || brval;
		break;
	case BIN_LXOR:
		assert(type_dealias(lvalue.result)->storage == TYPE_STORAGE_BOOL
			&& type_dealias(rvalue.result)->storage == TYPE_STORAGE_BOOL);
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
	} else if (type_dealias(in->result)->storage == TYPE_STORAGE_BOOL) {
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
	if (storage == TYPE_STORAGE_ENUM) {
		storage = type_dealias(out->result)->_enum.storage;
	}
	struct array_constant **next;
	switch (storage) {
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ENUM:
		assert(0); // Handled above
	case TYPE_STORAGE_ARRAY:
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
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_SLICE:
		assert(0); // TODO
	case TYPE_STORAGE_STRING:
		out->constant.string.len = in->constant.string.len;
		out->constant.string.value = xcalloc(1, in->constant.string.len);
		memcpy(out->constant.string.value,
			in->constant.string.value,
			in->constant.string.len);
		break;
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_TAGGED:
		assert(0); // TODO
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
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
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
	case TYPE_STORAGE_POINTER:
		if (from->storage == TYPE_STORAGE_NULL) {
			out->constant.uval = 0;
			return EVAL_OK;
		}
		assert(0); // TODO
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_SIZE:
		out->constant.uval = itrunc(to, val.constant.uval);
		return EVAL_OK;
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
		assert(val.result->storage == TYPE_STORAGE_ARRAY);
		out->constant = val.constant;
		return EVAL_OK;
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_TAGGED:
		assert(0); // TODO
	case TYPE_STORAGE_ALIAS:
		assert(0); // Handled above
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		assert(0); // Invariant
	case TYPE_STORAGE_VOID:
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
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_BOOL:
		break; // calloc does this for us
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		b.type = EXPR_STRUCT;
		b.result = v->result;
		b._struct.autofill = true;
		enum eval_result r = eval_expr(ctx, &b, v);
		assert(r == EVAL_OK);
		break;
	case TYPE_STORAGE_STRING:
		v->constant.string.value = strdup("");
		v->constant.string.len = 0;
		break;
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
		assert(0); // TODO
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_FUNCTION:
		assert(0); // Invariant
	case TYPE_STORAGE_VOID:
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
	assert(type_dealias(in->result)->storage != TYPE_STORAGE_UNION); // TODO
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

enum eval_result
eval_expr(struct context *ctx, struct expression *in, struct expression *out)
{
	switch (in->type) {
	case EXPR_ACCESS:
		assert(0); // TODO
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
	case EXPR_UNARITHM:
		assert(0); // TODO
	case EXPR_ALLOC:
	case EXPR_APPEND:
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINDING:
	case EXPR_BREAK:
	case EXPR_CONTINUE:
	case EXPR_CALL:
	case EXPR_DEFER:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
	case EXPR_LIST:
	case EXPR_MATCH:
	case EXPR_RETURN:
	case EXPR_SWITCH:
		// Excluded from translation-compatible subset
		return EVAL_INVALID;
	}
	assert(0); // Unreachable
}
