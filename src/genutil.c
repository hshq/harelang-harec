#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gen.h"
#include "qbe.h"
#include "types.h"
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
	struct qbe_value copy = mkqtmp(ctx, ctx->arch.ptr, fmt);
	pushi(ctx->current, &copy, Q_COPY, &qval, NULL);
	return copy;
}

struct qbe_value
mkqtmp(struct gen_context *ctx, const struct qbe_type *qtype, const char *fmt)
{
	return (struct qbe_value){
		.kind = QV_TEMPORARY,
		.type = qtype,
		.name = gen_name(ctx, fmt),
	};
}

struct gen_value
mkgtemp(struct gen_context *ctx, const struct type *type, const char *fmt)
{
	return (struct gen_value){
		.kind = GV_TEMP,
		.type = type,
		.name = gen_name(ctx, fmt),
	};
}

struct qbe_value
mkrtfunc(struct gen_context *ctx, const char *name)
{
	return (struct qbe_value){
		.kind = QV_GLOBAL,
		.name = strdup(name),
		.type = ctx->arch.ptr,
	};
}

struct qbe_value
mklabel(struct gen_context *ctx, struct qbe_statement *stmt, const char *fmt)
{
	struct qbe_value val;
	val.kind = QV_LABEL;
	val.name = strdup(genl(stmt, &ctx->id, fmt));
	return val;
}

void
branch_copyresult(struct gen_context *ctx,
	struct gen_value result,
	struct gen_value merged,
	struct gen_value *out)
{
	// Branching expressions written in the _with style may need to
	// consolodate each branch's result into a single temporary to return to
	// the caller. This function facilitates that.
	if (out
		|| type_dealias(merged.type)->storage == STORAGE_VOID
		|| type_dealias(result.type)->storage == STORAGE_VOID) {
		return;
	}
	struct qbe_value qmerged = mkqval(ctx, &merged);
	struct qbe_value qval = mkqval(ctx, &result);
	pushi(ctx->current, &qmerged, Q_COPY, &qval, NULL);
}
