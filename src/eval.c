#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "eval.h"
#include "expr.h"
#include "scope.h"
#include "type_store.h"
#include "types.h"
#include "util.h"

static void
error(struct context *ctx, const struct location loc, char *fmt, ...)
{
	va_list ap, copy;
	va_start(ap, fmt);

	va_copy(copy, ap);
	size_t sz = vsnprintf(NULL, 0, fmt, copy);
	va_end(copy);

	char *msg = xcalloc(1, sz + 1);
	vsnprintf(msg, sz + 1, fmt, ap);
	va_end(ap);

	struct errors *next = *ctx->next = xcalloc(1, sizeof(struct errors));
	next->loc = loc;
	next->msg = msg;
	ctx->next = &next->next;
}

static enum eval_result
eval_access(struct context *ctx, struct expression *in, struct expression *out)
{
	struct expression tmp = {0};
	enum eval_result r;

	switch (in->access.type) {
	case ACCESS_IDENTIFIER:
		return EVAL_INVALID; // &ident handled in eval_unarithm
	case ACCESS_INDEX:
		r = eval_expr(ctx, in->access.array, &tmp);
		if (r != EVAL_OK) {
			return r;
		}
		const struct array_constant *array = tmp.constant.array;
		r = eval_expr(ctx, in->access.index, &tmp);
		if (r != EVAL_OK) {
			return r;
		}
		for (size_t i = tmp.constant.uval; i > 0; --i) {
			if (array == NULL) {
				error(ctx, in->loc, "slice or array access out of bounds");
				return EVAL_INVALID;
			}
			array = array->next;
		}
		return eval_expr(ctx, array->value, out);
	case ACCESS_FIELD:
		r = eval_expr(ctx, in->access._struct, &tmp);
		if (r != EVAL_OK) {
			return r;
		}
		const struct struct_constant *fields = tmp.constant._struct;
		for (; fields != NULL; fields = fields->next) {
			if (!strcmp(fields->field->name, in->access.field->name)) {
				break;
			}
		}
		if (fields == NULL) {
			return EVAL_INVALID;
		}
		return eval_expr(ctx, fields->value, out);
	case ACCESS_TUPLE:
		r = eval_expr(ctx, in->access.tuple, &tmp);
		if (r != EVAL_OK) {
			return r;
		}
		const struct tuple_constant *tuple = tmp.constant.tuple;
		for (size_t i = in->access.tindex; i > 0; --i) {
			if (tuple == NULL) {
				// out of bounds
				return EVAL_INVALID;
			}
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
	case STORAGE_CHAR:
	case STORAGE_U8:
		return (uint8_t)val;
	case STORAGE_U16:
		return (uint16_t)val;
	case STORAGE_U32:
	case STORAGE_RCONST:
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
		return itrunc(type->alias.type, val);
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
	case STORAGE_VALIST:
	case STORAGE_VOID:
		assert(0);
	}
	assert(0);
}

static double
ftrunc(const struct type *type, double val)
{
	if (type->storage == STORAGE_F32) {
		return (float)val;
	}
	assert(type_is_float(type));
	return val;
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
	intmax_t ilval = lvalue.constant.ival, irval = rvalue.constant.ival, ival;
	uintmax_t ulval = lvalue.constant.uval, urval = rvalue.constant.uval, uval;
	double flval = lvalue.constant.fval, frval = rvalue.constant.fval, fval;
	// Type promotion is lowered in check
	assert(lvalue.result->storage == rvalue.result->storage);
	switch (in->binarithm.op) {
	case BIN_BAND:
		assert(!type_is_float(lvalue.result));
		if (type_is_signed(lvalue.result)) {
			ival = itrunc(lvalue.result, ilval) & itrunc(rvalue.result, irval);
		} else {
			uval = itrunc(lvalue.result, ulval) & itrunc(rvalue.result, urval);
		}
		break;
	case BIN_BOR:
		assert(!type_is_float(lvalue.result));
		if (type_is_signed(lvalue.result)) {
			ival = itrunc(lvalue.result, ilval) | itrunc(rvalue.result, irval);
		} else {
			uval = itrunc(lvalue.result, ulval) | itrunc(rvalue.result, urval);
		}
		break;
	case BIN_DIV:
		if (type_is_float(lvalue.result)) {
			fval = ftrunc(lvalue.result, flval) / ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			uintmax_t r = itrunc(rvalue.result, irval);
			if (r == 0) {
				error(ctx, in->loc, "division by zero");
				return EVAL_INVALID;
			}
			ival = itrunc(lvalue.result, ilval) / r;
		} else {
			uintmax_t r = itrunc(rvalue.result, urval);
			if (r == 0) {
				error(ctx, in->loc, "division by zero");
				return EVAL_INVALID;
			}
			uval = itrunc(lvalue.result, ulval) / r;
		}
		break;
	case BIN_LSHIFT:
		assert(!type_is_signed(rvalue.result));
		assert(!type_is_float(lvalue.result));
		uval = itrunc(lvalue.result, ulval) << itrunc(rvalue.result, urval);
		break;
	case BIN_MINUS:
		if (type_is_float(lvalue.result)) {
			fval = ftrunc(lvalue.result, flval) - ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			ival = itrunc(lvalue.result, ilval) - itrunc(rvalue.result, irval);
		} else {
			uval = itrunc(lvalue.result, ulval) - itrunc(rvalue.result, urval);
		}
		break;
	case BIN_MODULO:
		assert(!type_is_float(lvalue.result));
		if (type_is_signed(lvalue.result)) {
			uintmax_t r = itrunc(rvalue.result, irval);
			if (r == 0) {
				error(ctx, in->loc, "division by zero");
				return EVAL_INVALID;
			}
			ival = itrunc(lvalue.result, ilval) % itrunc(rvalue.result, irval);
		} else {
			uintmax_t r = itrunc(rvalue.result, urval);
			if (r == 0) {
				error(ctx, in->loc, "division by zero");
				return EVAL_INVALID;
			}
			uval = itrunc(lvalue.result, ulval) % itrunc(rvalue.result, urval);
		}
		break;
	case BIN_PLUS:
		if (type_is_float(lvalue.result)) {
			fval = ftrunc(lvalue.result, flval) + ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			ival = itrunc(lvalue.result, ilval) + itrunc(rvalue.result, irval);
		} else {
			uval = itrunc(lvalue.result, ulval) + itrunc(rvalue.result, urval);
		}
		break;
	case BIN_RSHIFT:
		assert(!type_is_signed(rvalue.result));
		assert(!type_is_float(lvalue.result));
		uval = itrunc(lvalue.result, ulval) >> itrunc(rvalue.result, urval);
		break;
	case BIN_TIMES:
		if (type_is_float(lvalue.result)) {
			fval = ftrunc(lvalue.result, flval) * ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			ival = itrunc(lvalue.result, ilval) * itrunc(rvalue.result, irval);
		} else {
			uval = itrunc(lvalue.result, ulval) * itrunc(rvalue.result, urval);
		}
		break;
	case BIN_BXOR:
		assert(!type_is_float(lvalue.result));
		if (type_is_signed(lvalue.result)) {
			ival = itrunc(lvalue.result, ilval) ^ itrunc(rvalue.result, irval);
		} else {
			uval = itrunc(lvalue.result, ulval) ^ itrunc(rvalue.result, urval);
		}
		break;
	// Logical arithmetic
	case BIN_GREATER:
		if (type_is_float(lvalue.result)) {
			bval = ftrunc(lvalue.result, flval) > ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			bval = itrunc(lvalue.result, ilval) > itrunc(rvalue.result, irval);
		} else {
			bval = itrunc(lvalue.result, ulval) > itrunc(rvalue.result, urval);
		}
		break;
	case BIN_GREATEREQ:
		if (type_is_float(lvalue.result)) {
			bval = ftrunc(lvalue.result, flval) >= ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			bval = itrunc(lvalue.result, ilval) >= itrunc(rvalue.result, irval);
		} else {
			bval = itrunc(lvalue.result, ulval) >= itrunc(rvalue.result, urval);
		}
		break;
	case BIN_LAND:
		assert(type_dealias(lvalue.result)->storage == STORAGE_BOOL
			&& type_dealias(rvalue.result)->storage == STORAGE_BOOL);
		bval = blval && brval;
		break;
	case BIN_LEQUAL:
		if (type_is_float(lvalue.result)) {
			bval = ftrunc(lvalue.result, flval) == ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			bval = itrunc(lvalue.result, ilval) == itrunc(rvalue.result, irval);
		} else {
			bval = itrunc(lvalue.result, ulval) == itrunc(rvalue.result, urval);
		}
		break;
	case BIN_LESS:
		if (type_is_float(lvalue.result)) {
			bval = ftrunc(lvalue.result, flval) < ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			bval = itrunc(lvalue.result, ilval) < itrunc(rvalue.result, irval);
		} else {
			bval = itrunc(lvalue.result, ulval) < itrunc(rvalue.result, urval);
		}
		break;
	case BIN_LESSEQ:
		if (type_is_float(lvalue.result)) {
			bval = ftrunc(lvalue.result, flval) <= ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			bval = itrunc(lvalue.result, ilval) <= itrunc(rvalue.result, irval);
		} else {
			bval = itrunc(lvalue.result, ulval) <= itrunc(rvalue.result, urval);
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
		if (type_is_float(lvalue.result)) {
			bval = ftrunc(lvalue.result, flval) != ftrunc(rvalue.result, frval);
		} else if (type_is_signed(lvalue.result)) {
			bval = itrunc(lvalue.result, ilval) != itrunc(rvalue.result, irval);
		} else {
			bval = itrunc(lvalue.result, ulval) != itrunc(rvalue.result, urval);
		}
		break;
	}
	if (type_is_float(in->result)) {
		out->constant.fval = ftrunc(in->result, fval);
	} else if (type_is_signed(in->result)) {
		out->constant.ival = itrunc(in->result, ival);
	} else if (type_dealias(in->result)->storage == STORAGE_BOOL) {
		out->constant.bval = bval;
	} else {
		out->constant.uval = itrunc(in->result, uval);
	}
	return EVAL_OK;
}

enum eval_result
eval_const(struct context *ctx, struct expression *in, struct expression *out)
{
	enum type_storage storage = type_dealias(out->result)->storage;
	if (storage == STORAGE_ENUM) {
		storage = type_dealias(out->result)->alias.type->storage;
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
	case STORAGE_TAGGED:
		out->constant.tagged.tag = in->constant.tagged.tag;
		out->constant.tagged.value = xcalloc(sizeof(struct expression), 1);
		return eval_expr(ctx, in->constant.tagged.value,
				out->constant.tagged.value);
	case STORAGE_STRUCT:
	case STORAGE_UNION:
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
	case STORAGE_RCONST:
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
	case STORAGE_VALIST:
		abort(); // Invariant
	}
	return EVAL_OK;
}

static void
eval_expand_array(struct context *ctx,
	const struct type *intype, const struct type *outtype,
	struct expression *in, struct expression *out)
{
	assert(in->type == EXPR_CONSTANT);
	assert(out->type == EXPR_CONSTANT);
	assert(intype->storage == STORAGE_ARRAY);
	assert(outtype->storage == STORAGE_ARRAY);
	struct array_constant *array_in = in->constant.array;
	struct array_constant **next = &out->constant.array;
	for (size_t i = 0; i < outtype->array.length; i++) {
		struct array_constant *item = *next =
			xcalloc(1, sizeof(struct array_constant));
		item->value = array_in->value;
		next = &item->next;
		if (array_in->next) {
			array_in = array_in->next;
		}
	}
}

enum eval_result
eval_type_assertion(struct context *ctx, struct expression *in,
		struct expression *out)
{
	struct expression val = {0};
	enum eval_result r = eval_expr(ctx, in->cast.value, &val);
	if (r != EVAL_OK) {
		return r;
	}

	const struct type *from = type_dealias(in->cast.value->result);
	assert(from->storage == STORAGE_TAGGED);
	if (val.constant.tagged.tag == in->cast.secondary) {
		out->constant = val.constant.tagged.value->constant;
		return EVAL_OK;
	} else {
		error(ctx, in->loc, "type assertion failed");
		return EVAL_INVALID;
	}
}

enum eval_result
eval_type_test(struct context *ctx, struct expression *in,
		struct expression *out)
{
	struct expression val = {0};
	enum eval_result r = eval_expr(ctx, in->cast.value, &val);
	if (r != EVAL_OK) {
		return r;
	}

	const struct type *from = type_dealias(in->cast.value->result);
	assert(from->storage == STORAGE_TAGGED);

	out->constant.bval = val.constant.tagged.tag == in->cast.secondary;

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
	// The STORAGE_ARRAY exception is to make sure we handle expandable
	// arrays at this point.
	if (to->storage == from->storage && to->storage != STORAGE_ARRAY) {
		out->constant = val.constant;
		return EVAL_OK;
	}

	// XXX: We should also be able to handle expressions which use
	// symbols/identifiers

	const struct type *subtype;
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
	case STORAGE_ENUM:
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
	case STORAGE_RCONST:
	case STORAGE_RUNE:
		if (type_is_float(val.result)) {
			out->constant.ival =
				itrunc(to, (intmax_t)val.constant.fval);
		} else if (type_is_signed(val.result)) {
			out->constant.ival = itrunc(to, val.constant.ival);
		} else {
			out->constant.ival = itrunc(to, val.constant.uval);
		}
		return EVAL_OK;
	case STORAGE_ARRAY:
		assert(from->storage == STORAGE_ARRAY);
		if (from->array.expandable) {
			eval_expand_array(ctx, from, to, &val, out);
		} else {
			out->constant = val.constant;
		}
		return EVAL_OK;
	case STORAGE_SLICE:
		assert(val.result->storage == STORAGE_ARRAY);
		out->constant = val.constant;
		return EVAL_OK;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		if (type_is_float(val.result)) {
			out->constant.fval = ftrunc(to, val.constant.fval);
		} else if (type_is_signed(val.result)) {
			out->constant.fval =
				ftrunc(to, (double)val.constant.ival);
		} else {
			out->constant.fval =
				ftrunc(to, (double)val.constant.uval);
		}
		return EVAL_OK;
	case STORAGE_TAGGED:
		subtype = tagged_select_subtype(to, val.result);
		out->constant.tagged.value =
			xcalloc(1, sizeof(struct expression));
		if (subtype) {
			out->constant.tagged.tag = subtype;
			*out->constant.tagged.value = val;
		} else {
			out->constant.tagged.tag = from;
			*out->constant.tagged.value = val;
		}
		return EVAL_OK;
	case STORAGE_CHAR:
	case STORAGE_NULL:
	case STORAGE_ALIAS:
		assert(0); // Handled above
	case STORAGE_BOOL:
	case STORAGE_FUNCTION:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
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
	struct expression obj = {0};
	enum eval_result res;
	switch (in->measure.op) {
	case M_LEN:
		res = eval_expr(ctx, in->measure.value, &obj);
		if (res != EVAL_OK) {
			return res;
		}

		switch (obj.result->storage) {
		case STORAGE_ARRAY:
		case STORAGE_SLICE:
			break;
		case STORAGE_STRING:
			out->constant.uval = obj.constant.string.len;
			return EVAL_OK;
		default:
			abort(); // Invariant
		}

		uintmax_t len = 0;
		for (struct array_constant *c = obj.constant.array;
				c != NULL; c = c->next) {
			len++;
		}
		out->constant.uval = len;
		return EVAL_OK;
	case M_SIZE:
		out->constant.uval = in->measure.dimensions.size;
		return EVAL_OK;
	case M_OFFSET:
		if (in->measure.value->access.type == ACCESS_FIELD) {
			out->constant.uval =
				in->measure.value->access.field->offset;
		} else {
			assert(in->measure.value->access.type == ACCESS_TUPLE);
			out->constant.uval =
				in->measure.value->access.tvalue->offset;
		}
		return EVAL_OK;
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
	case STORAGE_RCONST:
	case STORAGE_RUNE:
	case STORAGE_SLICE:
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
		v->constant.array = xcalloc(1, sizeof(struct array_constant));
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
	case STORAGE_VALIST:
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

static size_t
count_struct_fields(const struct type *type)
{
	size_t n = 0;
	assert(type->storage == STORAGE_STRUCT || type->storage == STORAGE_UNION);
	for (const struct struct_field *field = type->struct_union.fields;
			field; field = field->next) {
		if (!field->name) {
			n += count_struct_fields(type_dealias(field->type));
		} else {
			++n;
		}
	}
	return n;
}

void
autofill_struct(struct context *ctx, const struct type *type, struct struct_constant **fields)
{
	assert(type->storage == STORAGE_STRUCT || type->storage == STORAGE_UNION);
	for (const struct struct_field *field = type->struct_union.fields;
			field; field = field->next) {
		if (!field->name) {
			autofill_struct(ctx, type_dealias(field->type), fields);
			continue;
		}
		size_t i = 0;
		bool skip = false;
		for (; fields[i]; ++i) {
			if (!strcmp(field->name, fields[i]->field->name)) {
				skip = true;
				break;
			}
		}
		if (!skip) {
			fields[i] = xcalloc(1, sizeof(struct struct_constant));
			fields[i]->field = field;
			fields[i]->value = xcalloc(1, sizeof(struct expression));
			fields[i]->value->type = EXPR_CONSTANT;
			fields[i]->value->result = field->type;
			constant_default(ctx, fields[i]->value);
		}
	}
}

enum eval_result
eval_struct(struct context *ctx, struct expression *in, struct expression *out)
{
	assert(in->type == EXPR_STRUCT);
	assert(type_dealias(in->result)->storage != STORAGE_UNION); // TODO
	const struct type *type = type_dealias(in->result);

	size_t n = count_struct_fields(type);
	assert(n > 0);

	size_t i = 0;
	struct struct_constant **fields =
		xcalloc(n, sizeof(struct struct_constant *));
	for (const struct expr_struct_field *field_in = in->_struct.fields;
			field_in; field_in = field_in->next, ++i) {
		const struct struct_field *field =
			type_get_field(type, field_in->field->name);
		fields[i] = xcalloc(1, sizeof(struct struct_constant));
		fields[i]->field = field;
		fields[i]->value = xcalloc(1, sizeof(struct expression));

		enum eval_result r = eval_expr(ctx,
			field_in->value, fields[i]->value);
		if (r != EVAL_OK) {
			return r;
		}
	}
	assert(in->_struct.autofill || i == n);

	if (in->_struct.autofill) {
		autofill_struct(ctx, type, fields);
	}

	qsort(fields, n, sizeof(struct struct_constant *), field_compar);

	for (size_t i = 0; i < n - 1; ++i) {
		fields[i]->next = fields[i + 1];
	}

	out->constant._struct = fields[0];
	free(fields);
	return EVAL_OK;
}

static enum eval_result
eval_tuple(struct context *ctx, struct expression *in, struct expression *out)
{
	assert(in->type == EXPR_TUPLE);
	const struct type *type = type_dealias(in->result);

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
	return EVAL_OK;
}


static enum eval_result
eval_unarithm(struct context *ctx, struct expression *in, struct expression *out)
{
	if (in->unarithm.op == UN_ADDRESS) {
		assert(in->unarithm.operand->type == EXPR_ACCESS);
		// TODO other access types
		assert(in->unarithm.operand->access.type == ACCESS_IDENTIFIER);
		if (in->unarithm.operand->access.object->otype != O_DECL) {
			return EVAL_INVALID;
		}
		out->constant.object = in->unarithm.operand->access.object;
		out->constant.ival = 0;
		return EVAL_OK;
	}

	struct expression lvalue = {0};
	enum eval_result r = eval_expr(ctx, in->unarithm.operand, &lvalue);
	if (r != EVAL_OK) {
		return r;
	}

	switch (in->unarithm.op) {
	case UN_ADDRESS:
		assert(0); // handled above
	case UN_BNOT:
		out->constant.uval = ~lvalue.constant.uval;
		break;
	case UN_DEREF:
		assert(0); // TODO
	case UN_LNOT:
		out->constant.bval = !lvalue.constant.bval;
		break;
	case UN_MINUS:
		if (type_is_float(out->result)) {
			out->constant.fval = -lvalue.constant.fval;
		} else {
			out->constant.ival = -lvalue.constant.ival;
		}
		break;
	case UN_PLUS:
		out->constant = lvalue.constant;
		break;
	}

	return EVAL_OK;
}

enum eval_result
eval_expr(struct context *ctx, struct expression *in, struct expression *out)
{
	out->result = in->result;
	out->type = EXPR_CONSTANT;

	switch (in->type) {
	case EXPR_ACCESS:
		return eval_access(ctx, in, out);
	case EXPR_BINARITHM:
		return eval_binarithm(ctx, in, out);
	case EXPR_CAST:
		switch (in->cast.kind) {
		case C_CAST:
			return eval_cast(ctx, in, out);
		case C_ASSERTION:
			return eval_type_assertion(ctx, in, out);
		case C_TEST:
			return eval_type_test(ctx, in, out);
		default:
			assert(0); // Unreacheable
		}
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
	case EXPR_CALL:
	case EXPR_COMPOUND:
	case EXPR_CONTINUE:
	case EXPR_DEFER:
	case EXPR_DELETE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
	case EXPR_INSERT:
	case EXPR_MATCH:
	case EXPR_PROPAGATE:
	case EXPR_RETURN:
	case EXPR_SWITCH:
	case EXPR_VAARG:
	case EXPR_VAEND:
	case EXPR_VASTART:
	case EXPR_YIELD:
		error(ctx, in->loc, "unavailable at translation time");
		return EVAL_INVALID;
	}
	assert(0); // Unreachable
}
