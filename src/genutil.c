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

struct qbe_value
mklval(struct gen_context *ctx, struct gen_value *value)
{
	struct qbe_value qval = mkqval(ctx, value);
	qval.type = ctx->arch.ptr;
	return qval;
}

struct qbe_value
mkcopy(struct gen_context *ctx, struct gen_value *value, const char *fmt)
{
	struct qbe_value qval = mkqval(ctx, value);
	struct qbe_value copy = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = gen_name(ctx, fmt),
	};
	pushi(ctx->current, &copy, Q_COPY, &qval, NULL);
	return copy;
}

struct gen_value
mktemp(struct gen_context *ctx, const struct type *type, const char *fmt)
{
	return (struct gen_value){
		.kind = GV_TEMP,
		.type = type,
		.name = gen_name(ctx, fmt),
	};
}
