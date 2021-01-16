#include <assert.h>
#include <stdbool.h>
#include "eval.h"
#include "expr.h"
#include "scope.h"
#include "type_store.h"
#include "types.h"

enum eval_result
eval_const(struct context *ctx, struct expression *in, struct expression *out)
{
	// TODO: Update for aggregate types
	out->type = EXPR_CONSTANT;
	out->result = in->result;
	out->constant = in->constant;
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
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
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
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_TAGGED_UNION:
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
eval_expr(struct context *ctx, struct expression *in, struct expression *out)
{
	switch (in->type) {
	case EXPR_ACCESS:
	case EXPR_BINARITHM:
		assert(0); // TODO
	case EXPR_CAST:
		return eval_cast(ctx, in, out);
	case EXPR_CONSTANT:
		return eval_const(ctx, in, out);
	case EXPR_CONTINUE:
	case EXPR_MEASURE:
	case EXPR_SLICE:
	case EXPR_STRUCT:
	case EXPR_UNARITHM:
		assert(0); // TODO
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINDING:
	case EXPR_BREAK:
	case EXPR_CALL:
	case EXPR_FOR:
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
