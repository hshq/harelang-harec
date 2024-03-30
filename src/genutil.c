#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gen.h"
#include "qbe.h"
#include "types.h"
#include "util.h"

static struct qbe_value
mkrtfunc(struct gen_context *ctx, char *name)
{
	return (struct qbe_value){
		.kind = QV_GLOBAL,
		.name = name,
		.type = ctx->arch.ptr,
	};
}

void
rtfunc_init(struct gen_context *ctx)
{
	ctx->rt = (struct rt){
		.abort = mkrtfunc(ctx, "rt.abort"),
		.ensure = mkrtfunc(ctx, "rt.ensure"),
		.fixedabort = mkrtfunc(ctx, "rt.abort_fixed"),
		.free = mkrtfunc(ctx, "rt.free"),
		.malloc = mkrtfunc(ctx, "rt.malloc"),
		.memcpy = mkrtfunc(ctx, "rt.memcpy"),
		.memmove = mkrtfunc(ctx, "rt.memmove"),
		.memset = mkrtfunc(ctx, "rt.memset"),
		.strcmp = mkrtfunc(ctx, "rt.strcmp"),
		.unensure = mkrtfunc(ctx, "rt.unensure"),
	};
}

static struct qbe_value
mkval(const struct gen_value *value, const struct qbe_type *type)
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
		qval.threadlocal = value->threadlocal;
		break;
	case GV_TEMP:
		qval.kind = QV_TEMPORARY;
		qval.name = value->name;
		break;
	}
	qval.type = type;
	return qval;
}

struct qbe_value
mkqval(struct gen_context *ctx, const struct gen_value *value)
{
	return mkval(value, qtype_lookup(ctx, value->type, true));
}

struct qbe_value
mklval(struct gen_context *ctx, const struct gen_value *value)
{
	return mkval(value, ctx->arch.ptr);
}

struct qbe_value
mkcopy(struct gen_context *ctx, const struct gen_value *value, const char *fmt)
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
		.name = gen_name(&ctx->id, fmt),
	};
}

struct gen_value
mkgtemp(struct gen_context *ctx, const struct type *type, const char *fmt)
{
	return (struct gen_value){
		.kind = GV_TEMP,
		.type = type,
		.name = gen_name(&ctx->id, fmt),
	};
}

struct qbe_value
mklabel(struct gen_context *ctx, struct qbe_statement *stmt, const char *fmt)
{
	size_t n = snprintf(NULL, 0, fmt, ctx->id);
	char *l = xcalloc(1, n + 1);
	snprintf(l, n + 1, fmt, ctx->id);

	stmt->label = l;
	stmt->type = Q_LABEL;
	ctx->id++;
	return (struct qbe_value){
		.kind = QV_LABEL,
		.name = xstrdup(l),
	};
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
		|| type_dealias(NULL, merged.type)->storage == STORAGE_VOID
		|| type_dealias(NULL, merged.type)->storage == STORAGE_DONE
		|| merged.type->storage == STORAGE_NEVER
		|| type_dealias(NULL, result.type)->storage == STORAGE_VOID
		|| type_dealias(NULL, result.type)->storage == STORAGE_DONE
		|| result.type->storage == STORAGE_NEVER) {
		return;
	}
	struct qbe_value qmerged = mkqval(ctx, &merged);
	struct qbe_value qval = mkqval(ctx, &result);
	pushi(ctx->current, &qmerged, Q_COPY, &qval, NULL);
}

struct qbe_value
compute_tagged_memb_offset(const struct type *subtype)
{
	if (builtin_type_u32.align > subtype->align) {
		return constl(builtin_type_u32.align);
	}
	return constl(subtype->align);
}
