#include <stdio.h>
#include <stdlib.h>
#include "gen.h"
#include "qbe.h"
#include "util.h"

char *
gen_name(struct gen_context *ctx, const char *fmt)
{
	int n = snprintf(NULL, 0, fmt, ctx->id);
	char *str = xcalloc(1, n + 1);
	snprintf(str, n + 1, fmt, ctx->id);
	++ctx->id;
	return str;
}
