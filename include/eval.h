#ifndef HAREC_EVAL_H
#define HAREC_EVAL_H
#include <stdbool.h>

struct expression;
struct context;

// Evaluates an expression at compile time.
bool eval_expr(struct context *ctx, const struct expression *in,
	struct expression *out);

#endif
