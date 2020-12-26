#ifndef HAREC_EVAL_H
#define HAREC_EVAL_H
#include <stdbool.h>

struct expression;
struct context;

enum eval_result {
	// Evaluation succeeded.
	EVAL_OK,

	// Insufficient context, such as references to unknown types or
	// objects. Defer this expression until later and re-try when more of
	// the type & object graph are populated.
	EVAL_NEED_CONTEXT,

	// This expression cannot be evaluated at compile time (user error).
	EVAL_INVALID,
};

// Evaluates an expression at compile time.
enum eval_result eval_expr(struct context *ctx,
	struct expression *in, struct expression *out);

#endif
