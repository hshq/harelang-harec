#include <assert.h>
#include <stdbool.h>
#include "eval.h"
#include "expr.h"
#include "scope.h"
#include "type_store.h"
#include "types.h"

enum eval_result
eval_expr(struct context *ctx, struct expression *in, struct expression *out)
{
	switch (in->type) {
	case EXPR_ACCESS:
	case EXPR_BINARITHM:
	case EXPR_CAST:
	case EXPR_CONSTANT:
	case EXPR_CONTINUE:
	case EXPR_FOR:
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
	case EXPR_IF:
	case EXPR_LIST:
	case EXPR_MATCH:
	case EXPR_RETURN:
	case EXPR_SWITCH:
	case EXPR_WHILE:
		// Excluded from translation-compatible subset
		return EVAL_INVALID;
	}
	assert(0); // Unreachable
}
