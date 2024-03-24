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

static bool
eval_access(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	struct expression tmp = {0};
	switch (in->access.type) {
	case ACCESS_IDENTIFIER:
		return false; // &ident handled in eval_unarithm
	case ACCESS_INDEX:
		if (!eval_expr(ctx, in->access.array, &tmp)) {
			return false;
		}
		const struct array_literal *array = tmp.literal.array;
		if (!eval_expr(ctx, in->access.index, &tmp)) {
			return false;
		}
		for (size_t i = tmp.literal.uval; i > 0; --i) {
			if (array == NULL) {
				error(ctx, in->loc, NULL,
					"slice or array access out of bounds");
				return false;
			}
			array = array->next;
		}
		return eval_expr(ctx, array->value, out);
	case ACCESS_FIELD:
		if (!eval_expr(ctx, in->access._struct, &tmp)) {
			return false;
		}
		const struct struct_literal *fields = tmp.literal._struct;
		for (; fields != NULL; fields = fields->next) {
			if (!strcmp(fields->field->name, in->access.field->name)) {
				break;
			}
		}
		if (fields == NULL) {
			return false;
		}
		return eval_expr(ctx, fields->value, out);
	case ACCESS_TUPLE:
		if (!eval_expr(ctx, in->access.tuple, &tmp)) {
			return false;
		}
		const struct tuple_literal *tuple = tmp.literal.tuple;
		for (size_t i = in->access.tindex; i > 0; --i) {
			if (tuple == NULL) {
				// out of bounds
				return false;
			}
			tuple = tuple->next;
		}
		return eval_expr(ctx, tuple->value, out);
	}

	return true;
}

static uint64_t
itrunc(struct context *ctx, const struct type *type, uint64_t val)
{
	switch (type->storage) {
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
		return (int8_t)val;
	case STORAGE_I16:
		return (int16_t)val;
	case STORAGE_I32:
		return (int32_t)val;
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
		return itrunc(ctx, type_dealias(ctx, type), val);
	case STORAGE_ENUM:
		return itrunc(ctx, type->alias.type, val);
	case STORAGE_ERROR:
		return val;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
	case STORAGE_VOID:
	case STORAGE_DONE:
		assert(0);
	}
	assert(0);
}

static double
ftrunc(struct context *ctx, const struct type *type, double val)
{
	if (type->storage == STORAGE_F32) {
		return (float)val;
	}
	assert(type_is_float(ctx, type));
	return val;
}

static bool
eval_binarithm(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	struct expression lvalue = {0}, rvalue = {0};
	if (!eval_expr(ctx, in->binarithm.lvalue, &lvalue)) {
		return false;
	}
	if (!eval_expr(ctx, in->binarithm.rvalue, &rvalue)) {
		return false;
	}

	bool blval = false, brval = false, bval = false;
	int64_t ilval = 0, irval = 0, ival = 0;
	uint64_t ulval = 0, urval = 0, uval = 0;
	double flval = 0, frval = 0, fval = 0;
	if (type_is_float(ctx, lvalue.result)) {
		flval = lvalue.literal.fval, frval = rvalue.literal.fval;
	} else if (type_is_signed(ctx, lvalue.result)) {
		ilval = lvalue.literal.ival, irval = rvalue.literal.ival;
	} else if (type_is_integer(ctx, lvalue.result)) {
		ulval = lvalue.literal.uval, urval = rvalue.literal.uval;
	} else if (type_dealias(ctx, lvalue.result)->storage == STORAGE_BOOL) {
		blval = lvalue.literal.bval, brval = rvalue.literal.bval;
	}

	// Type promotion is lowered in check
	assert(lvalue.result->storage == rvalue.result->storage);
	bool neg = false;
	switch (in->binarithm.op) {
	case BIN_BAND:
		assert(type_is_integer(ctx, lvalue.result));
		if (type_is_signed(ctx, lvalue.result)) {
			ival = itrunc(ctx, lvalue.result, ilval) & itrunc(ctx, rvalue.result, irval);
		} else {
			uval = itrunc(ctx, lvalue.result, ulval) & itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_BOR:
		assert(type_is_integer(ctx, lvalue.result));
		if (type_is_signed(ctx, lvalue.result)) {
			ival = itrunc(ctx, lvalue.result, ilval) | itrunc(ctx, rvalue.result, irval);
		} else {
			uval = itrunc(ctx, lvalue.result, ulval) | itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_DIV:
		if (type_is_float(ctx, lvalue.result)) {
			fval = ftrunc(ctx, lvalue.result, flval) / ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			int64_t l = itrunc(ctx, lvalue.result, ilval);
			int64_t r = itrunc(ctx, rvalue.result, irval);
			if (r == 0) {
				error(ctx, in->loc, NULL, "division by zero");
				return false;
			} else if (r == -1) {
				uint64_t bit = lvalue.result->size * 8 - 1;
				uint64_t min = -((uint64_t)1 << bit);
				if (l == (int64_t)min) {
					error(ctx, in->loc, NULL,
						"division overflow");
					return false;
				}
			}
			ival = l / r;
		} else {
			assert(type_is_integer(ctx, lvalue.result));
			uint64_t r = itrunc(ctx, rvalue.result, urval);
			if (r == 0) {
				error(ctx, in->loc, NULL, "division by zero");
				return false;
			}
			uval = itrunc(ctx, lvalue.result, ulval) / r;
		}
		break;
	case BIN_LSHIFT:
		assert(type_is_integer(ctx, lvalue.result));
		assert(type_is_integer(ctx, rvalue.result));
		assert(!type_is_signed(ctx, rvalue.result));
		uval = itrunc(ctx, lvalue.result, ulval) << itrunc(ctx, rvalue.result, urval);
		break;
	case BIN_MINUS:
		if (type_is_float(ctx, lvalue.result)) {
			fval = ftrunc(ctx, lvalue.result, flval) - ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			ival = itrunc(ctx, lvalue.result, ilval) - itrunc(ctx, rvalue.result, irval);
		} else {
			assert(type_is_integer(ctx, lvalue.result));
			uval = itrunc(ctx, lvalue.result, ulval) - itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_MODULO:
		assert(type_is_integer(ctx, lvalue.result));
		if (type_is_signed(ctx, lvalue.result)) {
			int64_t l = itrunc(ctx, lvalue.result, ilval);
			int64_t r = itrunc(ctx, rvalue.result, irval);
			if (r == 0) {
				error(ctx, in->loc, NULL, "division by zero");
				return false;
			} else if (r == -1) {
				uint64_t bit = lvalue.result->size * 8 - 1;
				uint64_t min = -((uint64_t)1 << bit);
				if (l == (int64_t)min) {
					error(ctx, in->loc, NULL,
						"division overflow");
					return false;
				}
			}
			ival = l % r;
		} else {
			uint64_t r = itrunc(ctx, rvalue.result, urval);
			if (r == 0) {
				error(ctx, in->loc, NULL, "division by zero");
				return false;
			}
			uval = itrunc(ctx, lvalue.result, ulval) % r;
		}
		break;
	case BIN_PLUS:
		if (type_is_float(ctx, lvalue.result)) {
			fval = ftrunc(ctx, lvalue.result, flval) + ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			ival = itrunc(ctx, lvalue.result, ilval) + itrunc(ctx, rvalue.result, irval);
		} else {
			assert(type_is_integer(ctx, lvalue.result));
			uval = itrunc(ctx, lvalue.result, ulval) + itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_RSHIFT:
		assert(type_is_integer(ctx, lvalue.result));
		assert(type_is_integer(ctx, rvalue.result));
		assert(!type_is_signed(ctx, rvalue.result));
		uval = itrunc(ctx, lvalue.result, ulval) >> itrunc(ctx, rvalue.result, urval);
		break;
	case BIN_TIMES:
		if (type_is_float(ctx, lvalue.result)) {
			fval = ftrunc(ctx, lvalue.result, flval) * ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			ival = (int64_t)itrunc(ctx, lvalue.result, ilval)
				* (int64_t)itrunc(ctx, rvalue.result, irval);
		} else {
			assert(type_is_integer(ctx, lvalue.result));
			uval = itrunc(ctx, lvalue.result, ulval) * itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_BXOR:
		assert(type_is_integer(ctx, lvalue.result));
		if (type_is_signed(ctx, lvalue.result)) {
			ival = itrunc(ctx, lvalue.result, ilval) ^ itrunc(ctx, rvalue.result, irval);
		} else {
			uval = itrunc(ctx, lvalue.result, ulval) ^ itrunc(ctx, rvalue.result, urval);
		}
		break;
	// Logical arithmetic
	case BIN_GREATER:
		if (type_is_float(ctx, lvalue.result)) {
			bval = ftrunc(ctx, lvalue.result, flval) > ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			bval = (int64_t)itrunc(ctx, lvalue.result, ilval) > (int64_t)itrunc(ctx, rvalue.result, irval);
		} else {
			assert(type_is_integer(ctx, lvalue.result));
			bval = itrunc(ctx, lvalue.result, ulval) > itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_GREATEREQ:
		if (type_is_float(ctx, lvalue.result)) {
			bval = ftrunc(ctx, lvalue.result, flval) >= ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			bval = (int64_t)itrunc(ctx, lvalue.result, ilval) >= (int64_t)itrunc(ctx, rvalue.result, irval);
		} else {
			assert(type_is_integer(ctx, lvalue.result));
			bval = itrunc(ctx, lvalue.result, ulval) >= itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_LAND:
		assert(type_dealias(ctx, lvalue.result)->storage == STORAGE_BOOL
			&& type_dealias(ctx, rvalue.result)->storage == STORAGE_BOOL);
		bval = blval && brval;
		break;
	case BIN_NEQUAL:
		neg = true;
		/* fallthrough */
	case BIN_LEQUAL:
		if (type_is_float(ctx, lvalue.result)) {
			bval = ftrunc(ctx, lvalue.result, flval) == ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			bval = itrunc(ctx, lvalue.result, ilval) == itrunc(ctx, rvalue.result, irval);
		} else if (type_is_integer(ctx, lvalue.result)
				|| type_dealias(ctx, lvalue.result)->storage == STORAGE_POINTER) {
			bval = itrunc(ctx, lvalue.result, ulval) == itrunc(ctx, rvalue.result, urval);
		} else if (type_dealias(ctx, lvalue.result)->storage == STORAGE_BOOL) {
			bval = lvalue.literal.bval == rvalue.literal.bval;
		} else if (type_dealias(ctx, lvalue.result)->storage == STORAGE_RCONST
				|| type_dealias(ctx, lvalue.result)->storage == STORAGE_RUNE) {
			bval = lvalue.literal.rune == rvalue.literal.rune;
		} else {
			assert(type_dealias(ctx, lvalue.result)->storage == STORAGE_STRING);
			if (lvalue.literal.string.len != rvalue.literal.string.len) {
				bval = false;
			} else {
				bval = memcmp(lvalue.literal.string.value,
					rvalue.literal.string.value,
					lvalue.literal.string.len) == 0;
			}
		}
		bval = bval != neg;
		break;
	case BIN_LESS:
		if (type_is_float(ctx, lvalue.result)) {
			bval = ftrunc(ctx, lvalue.result, flval) < ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			bval = (int64_t)itrunc(ctx, lvalue.result, ilval) < (int64_t)itrunc(ctx, rvalue.result, irval);
		} else {
			assert(type_is_integer(ctx, lvalue.result));
			bval = itrunc(ctx, lvalue.result, ulval) < itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_LESSEQ:
		if (type_is_float(ctx, lvalue.result)) {
			bval = ftrunc(ctx, lvalue.result, flval) <= ftrunc(ctx, rvalue.result, frval);
		} else if (type_is_signed(ctx, lvalue.result)) {
			bval = (int64_t)itrunc(ctx, lvalue.result, ilval) <= (int64_t)itrunc(ctx, rvalue.result, irval);
		} else {
			assert(type_is_integer(ctx, lvalue.result));
			bval = itrunc(ctx, lvalue.result, ulval) <= itrunc(ctx, rvalue.result, urval);
		}
		break;
	case BIN_LOR:
		assert(type_dealias(ctx, lvalue.result)->storage == STORAGE_BOOL
			&& type_dealias(ctx, rvalue.result)->storage == STORAGE_BOOL);
		bval = blval || brval;
		break;
	case BIN_LXOR:
		assert(type_dealias(ctx, lvalue.result)->storage == STORAGE_BOOL
			&& type_dealias(ctx, rvalue.result)->storage == STORAGE_BOOL);
		bval = blval != brval;
		break;
	}
	if (type_is_float(ctx, in->result)) {
		out->literal.fval = ftrunc(ctx, in->result, fval);
	} else if (type_is_signed(ctx, in->result)) {
		out->literal.ival = itrunc(ctx, in->result, ival);
	} else if (type_dealias(ctx, in->result)->storage == STORAGE_BOOL
			|| type_dealias(ctx, in->result)->storage == STORAGE_STRING) {
		out->literal.bval = bval;
	} else {
		assert(type_is_integer(ctx, in->result)
			|| type_dealias(ctx, in->result)->storage == STORAGE_POINTER);
		out->literal.uval = itrunc(ctx, in->result, uval);
	}
	return true;
}

static bool
eval_literal(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	enum type_storage storage = type_dealias(ctx, out->result)->storage;
	if (storage == STORAGE_ENUM) {
		storage = type_dealias(ctx, out->result)->alias.type->storage;
	}
	switch (storage) {
	case STORAGE_ALIAS:
	case STORAGE_ENUM:
		assert(0); // Handled above
	case STORAGE_ARRAY:;
		struct array_literal **anext = &out->literal.array;
		for (struct array_literal *arr = in->literal.array; arr;
				arr = arr->next) {
			struct array_literal *alit = *anext =
				xcalloc(1, sizeof(struct array_literal));
			alit->value = xcalloc(1, sizeof(struct expression));
			if (!eval_expr(ctx, arr->value, alit->value)) {
				return false;
			}
			anext = &alit->next;
		}
		break;
	case STORAGE_STRING:
		out->literal.string.len = in->literal.string.len;
		out->literal.string.value = xcalloc(1, in->literal.string.len);
		memcpy(out->literal.string.value,
			in->literal.string.value,
			in->literal.string.len);
		break;
	case STORAGE_TAGGED:
		out->literal.tagged.tag = in->literal.tagged.tag;
		out->literal.tagged.value = xcalloc(sizeof(struct expression), 1);
		return eval_expr(ctx, in->literal.tagged.value,
				out->literal.tagged.value);
	case STORAGE_STRUCT:;
		struct struct_literal **next = &out->literal._struct;
		for (struct struct_literal *_struct = in->literal._struct;
				_struct; _struct = _struct->next) {
			struct struct_literal *cur = *next =
				xcalloc(sizeof(struct struct_literal), 1);
			cur->field = _struct->field;
			cur->value = xcalloc(sizeof(struct expression), 1);
			if (!eval_expr(ctx, _struct->value, cur->value)) {
				return false;
			}
			next = &cur->next;
		}
		break;
	case STORAGE_UNION:
		assert(0); // TODO
	case STORAGE_TUPLE:;
		struct tuple_literal **tnext = &out->literal.tuple;
		for (struct tuple_literal *tuple = in->literal.tuple; tuple;
				tuple = tuple->next) {
			struct tuple_literal *tconst = *tnext =
				xcalloc(1, sizeof(struct tuple_literal));
			tconst->field = tuple->field;
			tconst->value = xcalloc(1, sizeof(struct expression));
			if (!eval_expr(ctx, tuple->value, tconst->value)) {
				return false;
			}
			tnext = &tconst->next;
		}
		break;
	case STORAGE_BOOL:
	case STORAGE_ERROR:
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
	case STORAGE_DONE:
		out->literal = in->literal;
		break;
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
	case STORAGE_SLICE:
	case STORAGE_VALIST:
		abort(); // Invariant
	}
	return true;
}

static void
eval_expand_array(struct context *ctx,
	const struct type *intype, const struct type *outtype,
	const struct expression *in, struct expression *out)
{
	assert(in->type == EXPR_LITERAL);
	assert(out->type == EXPR_LITERAL);
	assert(intype->storage == STORAGE_ARRAY);
	assert(outtype->storage == STORAGE_ARRAY);
	struct array_literal *array_in = in->literal.array;
	struct array_literal **next = &out->literal.array;
	for (size_t i = 0; i < outtype->array.length; i++) {
		struct array_literal *item = *next =
			xcalloc(1, sizeof(struct array_literal));
		item->value = array_in->value;
		next = &item->next;
		if (array_in->next) {
			array_in = array_in->next;
		}
	}
}

static bool
eval_type_assertion(struct context *ctx, const struct expression *in,
		struct expression *out)
{
	struct expression val = {0};
	if (!eval_expr(ctx, in->cast.value, &val)) {
		return false;
	}

	const struct type *from = type_dealias(ctx, in->cast.value->result);
	assert(from->storage == STORAGE_TAGGED);
	if (val.literal.tagged.tag == in->cast.secondary) {
		out->literal = val.literal.tagged.value->literal;
		return true;
	} else {
		error(ctx, in->loc, NULL, "type assertion failed");
		return false;
	}
}

static bool
eval_type_test(struct context *ctx, const struct expression *in,
		struct expression *out)
{
	struct expression val = {0};
	if (!eval_expr(ctx, in->cast.value, &val)) {
		return false;
	}

	const struct type *from = type_dealias(ctx, in->cast.value->result);
	assert(from->storage == STORAGE_TAGGED);

	out->literal.bval = val.literal.tagged.tag == in->cast.secondary;

	return true;
}

static bool
eval_cast(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	struct expression val = {0};
	if (!eval_expr(ctx, in->cast.value, &val)) {
		return false;
	}

	const struct type *to = type_dealias(ctx, in->result),
		*from = type_dealias(ctx, val.result);
	// The STORAGE_ARRAY exception is to make sure we handle expandable
	// arrays at this point.
	if (to->storage == from->storage && to->storage != STORAGE_ARRAY) {
		out->literal = val.literal;
		return true;
	}

	if (from->storage == STORAGE_ERROR) {
		return true;
	} else if (from->storage == STORAGE_TAGGED) {
		out->literal = val.literal.tagged.value->literal;
		return true;
	}

	// XXX: We should also be able to handle expressions which use
	// symbols/identifiers

	const struct type *subtype;
	switch (to->storage) {
	case STORAGE_POINTER:
		if (from->storage == STORAGE_NULL) {
			out->literal.uval = 0;
			return true;
		}
		assert(from->storage == STORAGE_POINTER
			|| from->storage == STORAGE_UINTPTR);
		out->literal.uval = val.literal.uval;
		return true;
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
		if (type_is_float(ctx, val.result)) {
			out->literal.ival =
				itrunc(ctx, to, (int64_t)val.literal.fval);
		} else if (type_is_signed(ctx, val.result)) {
			out->literal.ival = itrunc(ctx, to, val.literal.ival);
		} else {
			out->literal.ival = itrunc(ctx, to, val.literal.uval);
		}
		return true;
	case STORAGE_ARRAY:
		assert(from->storage == STORAGE_ARRAY);
		if (from->array.expandable) {
			eval_expand_array(ctx, from, to, &val, out);
		} else {
			out->literal = val.literal;
		}
		return true;
	case STORAGE_SLICE:
		assert(type_dealias(ctx, val.result)->storage == STORAGE_ARRAY);
		out->literal = val.literal;
		return true;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		if (type_is_float(ctx, val.result)) {
			out->literal.fval = ftrunc(ctx, to, val.literal.fval);
		} else if (type_is_signed(ctx, val.result)) {
			out->literal.fval =
				ftrunc(ctx, to, (double)val.literal.ival);
		} else {
			out->literal.fval =
				ftrunc(ctx, to, (double)val.literal.uval);
		}
		return true;
	case STORAGE_TAGGED:
		subtype = tagged_select_subtype(ctx, to, val.result, true);
		out->literal.tagged.value =
			xcalloc(1, sizeof(struct expression));
		if (subtype) {
			out->literal.tagged.tag = subtype;
			*out->literal.tagged.value = val;
		} else {
			out->literal.tagged.tag = from;
			*out->literal.tagged.value = val;
		}
		return true;
	case STORAGE_NULL:
	case STORAGE_ALIAS:
		assert(0); // Handled above
	case STORAGE_BOOL:
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
		assert(0); // Invariant
	case STORAGE_ERROR:
	case STORAGE_VOID:
	case STORAGE_DONE:
		return true;
	}

	assert(0); // Unreachable
}

static bool
eval_len(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	assert(in->type == EXPR_LEN);
	const struct type *expr_type = type_dereference(ctx, in->len.value->result);
	assert(expr_type != NULL);
	expr_type = type_dealias(ctx, expr_type);

	struct expression obj = {0};
	if (!eval_expr(ctx, in->len.value, &obj)) {
		return false;
	}

	switch (obj.result->storage) {
	case STORAGE_SLICE:
		break;
	case STORAGE_STRING:
		out->literal.uval = obj.literal.string.len;
		return true;
	case STORAGE_ERROR:
		out->literal.uval = 0;
		return true;
	case STORAGE_ARRAY:
	default:
		abort(); // Invariant
	}

	uint64_t len = 0;
	for (struct array_literal *c = obj.literal.array;
			c != NULL; c = c->next) {
		len++;
	}
	out->literal.uval = len;
	return true;
}

static bool
literal_default(struct context *ctx, struct expression *v)
{
	struct expression b = {0};
	switch (type_dealias(ctx, v->result)->storage) {
	case STORAGE_ERROR:
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
		bool r = eval_expr(ctx, &b, v);
		assert(r);
		break;
	case STORAGE_STRING:
		v->literal.string.value = NULL;
		v->literal.string.len = 0;
		break;
	case STORAGE_ARRAY:
		v->literal.array = xcalloc(1, sizeof(struct array_literal));
		v->literal.array->value = xcalloc(1, sizeof(struct expression));
		v->literal.array->value->type = EXPR_LITERAL;
		v->literal.array->value->result =
			type_dealias(ctx, v->result)->array.members;
		return literal_default(ctx, v->literal.array->value);
		break;
	case STORAGE_TAGGED:
		return false;
	case STORAGE_TUPLE:;
		struct tuple_literal **c = &v->literal.tuple;
		for (const struct type_tuple *t = &type_dealias(ctx, v->result)->tuple;
				t != NULL; t = t->next) {
			*c = xcalloc(1, sizeof(struct tuple_literal));
			(*c)->field = t;
			(*c)->value = xcalloc(1, sizeof(struct expression));
			(*c)->value->type = EXPR_LITERAL;
			(*c)->value->result = t->type;
			if (!literal_default(ctx, (*c)->value)) {
				return false;
			}
			c = &(*c)->next;
		}
		break;
	case STORAGE_ALIAS:
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
	case STORAGE_VALIST:
		assert(0); // Invariant
	case STORAGE_VOID:
	case STORAGE_DONE:
		break; // no-op
	}

	return true;
}

static int
field_compar(const void *_a, const void *_b)
{
	const struct struct_literal **a = (const struct struct_literal **)_a;
	const struct struct_literal **b = (const struct struct_literal **)_b;
	return (*a)->field->offset - (*b)->field->offset;
}

static size_t
count_struct_fields(struct context *ctx, const struct type *type)
{
	size_t n = 0;
	assert(type->storage == STORAGE_STRUCT || type->storage == STORAGE_UNION);
	for (const struct struct_field *field = type->struct_union.fields;
			field; field = field->next) {
		if (!field->name) {
			n += count_struct_fields(ctx, type_dealias(ctx, field->type));
		} else {
			++n;
		}
	}
	return n;
}

static bool
autofill_struct(struct context *ctx, const struct type *type, struct struct_literal **fields)
{
	assert(type->storage == STORAGE_STRUCT || type->storage == STORAGE_UNION);
	for (const struct struct_field *field = type->struct_union.fields;
			field; field = field->next) {
		if (!field->name) {
			bool r = autofill_struct(ctx,
				type_dealias(ctx, field->type), fields);
			if (!r) {
				return false;
			}
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
			fields[i] = xcalloc(1, sizeof(struct struct_literal));
			fields[i]->field = field;
			fields[i]->value = xcalloc(1, sizeof(struct expression));
			fields[i]->value->type = EXPR_LITERAL;
			fields[i]->value->result = field->type;
			// TODO: there should probably be a better error message
			// when this happens
			if (!literal_default(ctx, fields[i]->value)) {
				return false;
			}
		}
	}

	return true;
}

static bool
eval_struct(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	assert(in->type == EXPR_STRUCT);
	assert(type_dealias(ctx, in->result)->storage != STORAGE_UNION); // TODO
	const struct type *type = type_dealias(ctx, in->result);

	size_t n = count_struct_fields(ctx, type);
	assert(n > 0);

	size_t i = 0;
	struct struct_literal **fields =
		xcalloc(n, sizeof(struct struct_literal *));
	for (const struct expr_struct_field *field_in = in->_struct.fields;
			field_in; field_in = field_in->next, ++i) {
		const struct struct_field *field =
			type_get_field(ctx, type, field_in->field->name);
		fields[i] = xcalloc(1, sizeof(struct struct_literal));
		fields[i]->field = field;
		fields[i]->value = xcalloc(1, sizeof(struct expression));

		if (!eval_expr(ctx, field_in->value, fields[i]->value)) {
			return false;
		}
	}
	assert(in->_struct.autofill || i == n);

	if (in->_struct.autofill) {
		if (!autofill_struct(ctx, type, fields)) {
			return false;
		}
	}

	qsort(fields, n, sizeof(struct struct_literal *), field_compar);

	for (size_t i = 0; i < n - 1; ++i) {
		fields[i]->next = fields[i + 1];
	}

	out->literal._struct = fields[0];
	free(fields);
	return true;
}

static bool
eval_tuple(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	assert(in->type == EXPR_TUPLE);
	const struct type *type = type_dealias(ctx, in->result);

	struct tuple_literal *out_tuple_start, *out_tuple;
	out_tuple_start = out_tuple = xcalloc(1, sizeof(struct tuple_literal));
	const struct expression_tuple *in_tuple = &in->tuple;
	for (const struct type_tuple *field_type = &type->tuple; field_type;
			field_type = field_type->next) {
		out_tuple->value = xcalloc(1, sizeof(struct expression));
		if (!eval_expr(ctx, in_tuple->value, out_tuple->value)) {
			return false;
		}
		out_tuple->field = field_type;
		if (in_tuple->next) {
			in_tuple = in_tuple->next;
			out_tuple->next =
				xcalloc(1, sizeof(struct tuple_literal));
			out_tuple = out_tuple->next;
		}
	}

	out->literal.tuple = out_tuple_start;
	return true;
}


static bool
eval_unarithm(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	if (in->unarithm.op == UN_ADDRESS) {
		if (in->unarithm.operand->result == &builtin_type_error) {
			out->type = EXPR_LITERAL;
			out->result = &builtin_type_error;
			out->literal.uval = 0;
			return true;
		}
		if (in->unarithm.operand->type != EXPR_ACCESS) {
			return false;
		}
		const struct expression_access *access =
			&in->unarithm.operand->access;
		struct expression new_in = {0};
		switch (access->type) {
		case ACCESS_IDENTIFIER:
			if (access->object->otype != O_DECL) {
				return false;
			}
			out->literal.object = access->object;
			out->literal.ival = 0;
			return true;
		case ACCESS_INDEX:
			new_in = *in;
			new_in.unarithm.operand = access->array;
			if (!eval_expr(ctx, &new_in, out)) {
				return false;
			}
			struct expression index = {0};
			if (!eval_expr(ctx, access->index, &index)) {
				return false;
			}
			out->literal.ival += index.literal.uval * type_dealias(ctx,
				access->array->result)->array.members->size;
			return true;
		case ACCESS_FIELD:
			new_in = *in;
			new_in.unarithm.operand = access->_struct;
			if (!eval_expr(ctx, &new_in, out)) {
				return false;
			}
			out->literal.ival += access->field->offset;
			return true;
		case ACCESS_TUPLE:
			new_in = *in;
			new_in.unarithm.operand = access->tuple;
			if (!eval_expr(ctx, &new_in, out)) {
				return false;
			}
			out->literal.ival += access->tvalue->offset;
			return true;
		}
	}

	struct expression lvalue = {0};
	if (!eval_expr(ctx, in->unarithm.operand, &lvalue)) {
		return false;
	}

	switch (in->unarithm.op) {
	case UN_ADDRESS:
		assert(0); // handled above
	case UN_BNOT:
		out->literal.uval = ~lvalue.literal.uval;
		break;
	case UN_DEREF:
		return false;
	case UN_LNOT:
		out->literal.bval = !lvalue.literal.bval;
		break;
	case UN_MINUS:
		if (type_is_float(ctx, out->result)) {
			out->literal.fval = -lvalue.literal.fval;
		} else {
			out->literal.ival = -(uint64_t)lvalue.literal.ival;
		}
		break;
	}

	return true;
}

bool
eval_expr(struct context *ctx,
	const struct expression *in,
	struct expression *out)
{
	out->result = in->result;
	out->type = EXPR_LITERAL;

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
	case EXPR_LEN:
		return eval_len(ctx, in, out);
	case EXPR_LITERAL:
		return eval_literal(ctx, in, out);
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
	case EXPR_DEFINE:
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
		return false;
	}
	assert(0); // Unreachable
}
