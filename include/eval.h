#ifndef HAREC_EVAL_H
#define HAREC_EVAL_H
#include <stdbool.h>

struct expression;
struct context;

enum eval_result {
	// Evaluation succeeded.
	EVAL_OK,

	// This expression cannot be evaluated at compile time (user error).
	EVAL_INVALID,
};

// Evaluates an expression at compile time.
enum eval_result eval_expr(struct context *ctx,
	const struct expression *in, struct expression *out);

#endif
