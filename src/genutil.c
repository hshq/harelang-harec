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

struct qbe_value
mkqval(struct gen_context *ctx, struct gen_value *value)
{
	struct qbe_value qval = {0};
	switch (value->kind) {
	case GV_CONST:
		qval.kind = QV_CONST;
		qval.lval = value->lval; // XXX: Kind of hacky
		break;
	case GV_GLOBAL:
		qval.kind = QV_GLOBAL;
		qval.name = value->name;
		break;
	case GV_TEMP:
		qval.kind = QV_TEMPORARY;
		qval.name = value->name;
		break;
	}
	qval.type = qtype_lookup(ctx, value->type, true);
	return qval;
}
