#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "scope.h"
#include "typedef.h"
#include "types.h"
#include "util.h"

#define EXPR_GEN_VALUE -1

static const struct gen_value gv_void = {
	.kind = GV_CONST,
	.type = &builtin_type_void,
};

static struct gen_value gen_expr(struct gen_context *ctx,
	const struct expression *expr);
static void gen_expr_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out);
static struct gen_value gen_expr_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out);
static void gen_global_decl(struct gen_context *ctx,
	const struct declaration *decl);

static struct gen_scope *push_scope(struct gen_context *ctx,
	const struct scope *scope);
static void pop_scope(struct gen_context *ctx, bool gendefers);

static void
gen_defers(struct gen_context *ctx, struct gen_scope *scope)
{
	if (!scope) {
		return;
	}
	if (scope->defers) {
		pushc(ctx->current, "gen defers");
	}
	struct gen_defer *defers = scope->defers;
	while (scope->defers) {
		struct gen_defer *defer = scope->defers;
		assert(defer->expr->type == EXPR_DEFER);
		scope->defers = scope->defers->next;
		push_scope(ctx, defer->expr->defer.scope);
		gen_expr(ctx, defer->expr->defer.deferred);
		pop_scope(ctx, false);
	}
	scope->defers = defers;
}

static struct gen_scope *
gen_scope_lookup(struct gen_context *ctx, const struct scope *which)
{
	for (struct gen_scope *scope = ctx->scope;
			scope; scope = scope->parent) {
		if (scope->scope == which) {
			return scope;
		}
	}
	abort();
}

static struct gen_scope *
push_scope(struct gen_context *ctx, const struct scope *scope)
{
	struct gen_scope *new = xcalloc(1, sizeof(struct gen_scope));
	new->parent = ctx->scope;
	new->scope = scope;
	ctx->scope = new;
	return new;
}

static void
pop_scope(struct gen_context *ctx, bool gendefers)
{
	if (gendefers) {
		gen_defers(ctx, ctx->scope);
	}
	struct gen_scope *scope = ctx->scope;
	ctx->scope = scope->parent;
	for (struct gen_defer *defer = scope->defers; defer; /* n/a */) {
		struct gen_defer *next = defer->next;
		free(defer);
		defer = next;
	}
	free(scope);
}

static void
gen_copy_memcpy(struct gen_context *ctx,
	struct gen_value dest, struct gen_value src)
{
	struct qbe_value dtemp = mklval(ctx, &dest);
	struct qbe_value stemp = mklval(ctx, &src);
	struct qbe_value sz = constl(dest.type->size);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &dtemp, &stemp, &sz, NULL);
}

static void
gen_copy_aligned(struct gen_context *ctx,
	struct gen_value dest, struct gen_value src)
{
	if (dest.type->size > 128) {
		gen_copy_memcpy(ctx, dest, src);
		return;
	}
	struct qbe_value srcv = mkqval(ctx, &src);
	struct qbe_value destv = mkqval(ctx, &dest);
	struct qbe_value size = constl(dest.type->size);
	pushi(ctx->current, NULL, Q_BLIT, &srcv, &destv, &size, NULL);
}

static void
gen_store(struct gen_context *ctx,
	struct gen_value object,
	struct gen_value value)
{
	const struct type *ty = type_dealias(NULL, object.type);
	switch (ty->storage) {
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_VALIST:
		gen_copy_aligned(ctx, object, value);
		return;
	case STORAGE_ENUM:
		object.type = ty->alias.type;
		break;
	default:
		break; // no-op
	}

	struct qbe_value qobj = mkqval(ctx, &object),
		qval = mkqval(ctx, &value);
	enum qbe_instr qi = store_for_type(ctx, object.type);
	pushi(ctx->current, NULL, qi, &qval, &qobj, NULL);
}

static struct gen_value
gen_load(struct gen_context *ctx, struct gen_value object)
{
	const struct type *ty = type_dealias(NULL, object.type);
	switch (ty->storage) {
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
		return object;
	case STORAGE_ENUM:
		object.type = ty->alias.type;
		break;
	default:
		break; // no-op
	}

	struct gen_value value = {
		.kind = GV_TEMP,
		.type = object.type,
		.name = gen_name(&ctx->id, "load.%d"),
	};
	struct qbe_value qobj = mkqval(ctx, &object),
		qval = mkqval(ctx, &value);
	enum qbe_instr qi = load_for_type(ctx, object.type);
	pushi(ctx->current, &qval, qi, &qobj, NULL);
	return value;
}

static void
gen_fixed_abort(struct gen_context *ctx,
	struct location loc, enum fixed_aborts reason)
{
	for (struct gen_scope *scope = ctx->scope; scope; scope = scope->parent) {
		gen_defers(ctx, scope);
		if (scope->scope->class == SCOPE_DEFER) {
			break;
		}
	}

	struct expression eloc;
	mkstrconst(&eloc, "%s:%d:%d", sources[loc.file], loc.lineno, loc.colno);
	struct gen_value msg = gen_expr(ctx, &eloc);
	struct qbe_value qmsg = mkqval(ctx, &msg);
	struct qbe_value tmp = constl(reason);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.fixedabort, &qmsg, &tmp, NULL);
	pushi(ctx->current, NULL, Q_HLT, NULL);
}

static struct gen_value
gen_autoderef(struct gen_context *ctx, struct gen_value val)
{
	while (type_dealias(NULL, val.type)->storage == STORAGE_POINTER) {
		val.type = type_dealias(NULL, val.type)->pointer.referent;
		val = gen_load(ctx, val);
	}
	return val;
}

static struct gen_value
gen_access_ident(struct gen_context *ctx, const struct scope_object *obj)
{
	switch (obj->otype) {
	case O_BIND:
		for (const struct gen_binding *gb = ctx->bindings;
				gb; gb = gb->next) {
			if (gb->object == obj) {
				return gb->value;
			}
		}
		break;
	case O_DECL:
		return (struct gen_value){
			.kind = GV_GLOBAL,
			.type = obj->type,
			.name = ident_to_sym(&obj->ident),
			.threadlocal = obj->threadlocal,
		};
	case O_CONST:
	case O_TYPE:
	case O_SCAN:
		abort(); // Invariant
	}
	abort(); // Invariant
}

static struct gen_value
gen_access_index(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value glval = gen_expr(ctx, expr->access.array);
	glval = gen_autoderef(ctx, glval);
	struct qbe_value qlval = mkqval(ctx, &glval);
	struct qbe_value qival = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	bool checkbounds = !expr->access.bounds_checked;
	struct qbe_value length;
	const struct type *ty = type_dealias(NULL, glval.type);
	switch (ty->storage) {
	case STORAGE_SLICE:;
		enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
		struct qbe_value base = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &base, load, &qlval, NULL);

		struct qbe_value temp = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		length = mkqtmp(ctx, ctx->arch.sz, "len.%d");
		struct qbe_value offset = constl(builtin_type_size.size);
		pushi(ctx->current, &temp, Q_ADD, &qlval, &offset, NULL);
		pushi(ctx->current, &length, load, &temp, NULL);

		qlval = base;
		break;
	case STORAGE_ARRAY:
		if (ty->array.length != SIZE_UNDEFINED) {
			length = constl(ty->array.length);
		} else {
			checkbounds = false;
		}
		break;
	default:
		assert(0); // Unreachable
	}

	struct gen_value index = gen_expr(ctx, expr->access.index);
	struct qbe_value qindex = mkqval(ctx, &index);
	struct qbe_value itemsz = constl(expr->result->size);
	pushi(ctx->current, &qival, Q_MUL, &qindex, &itemsz, NULL);
	pushi(ctx->current, &qival, Q_ADD, &qlval, &qival, NULL);

	if (checkbounds) {
		struct qbe_value valid = mkqtmp(ctx, &qbe_word, ".%d");
		pushi(ctx->current, &valid, Q_CULTL, &qindex, &length, NULL);

		struct qbe_statement linvalid, lvalid;
		struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
		struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");

		pushi(ctx->current, NULL, Q_JNZ, &valid, &bvalid, &binvalid, NULL);
		push(&ctx->current->body, &linvalid);
		gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
		push(&ctx->current->body, &lvalid);
	}

	return (struct gen_value){
		.kind = GV_TEMP,
		.type = expr->result,
		.name = qival.name,
	};
}

static struct gen_value
gen_access_field(struct gen_context *ctx, const struct expression *expr)
{
	const struct struct_field *field = expr->access.field;
	struct gen_value glval = gen_expr(ctx, expr->access._struct);
	glval = gen_autoderef(ctx, glval);
	struct qbe_value qlval = mkqval(ctx, &glval);
	struct qbe_value qfval = mkqtmp(ctx, ctx->arch.ptr, "field.%d");
	struct qbe_value offs = constl(field->offset);
	pushi(ctx->current, &qfval, Q_ADD, &qlval, &offs, NULL);
	return (struct gen_value){
		.kind = GV_TEMP,
		.type = field->type,
		.name = qfval.name,
	};
}

static struct gen_value
gen_access_value(struct gen_context *ctx, const struct expression *expr)
{
	const struct type_tuple *tuple = expr->access.tvalue;
	struct gen_value glval = gen_expr(ctx, expr->access.tuple);
	glval = gen_autoderef(ctx, glval);
	struct qbe_value qlval = mkqval(ctx, &glval);
	struct qbe_value qfval = mkqtmp(ctx, ctx->arch.ptr, "value.%d");
	struct qbe_value offs = constl(tuple->offset);
	pushi(ctx->current, &qfval, Q_ADD, &qlval, &offs, NULL);
	return (struct gen_value){
		.kind = GV_TEMP,
		.type = tuple->type,
		.name = qfval.name,
	};
}

static struct gen_value
gen_expr_access_addr(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value addr;
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		addr = gen_access_ident(ctx, expr->access.object);
		break;
	case ACCESS_INDEX:
		addr = gen_access_index(ctx, expr);
		break;
	case ACCESS_FIELD:
		addr = gen_access_field(ctx, expr);
		break;
	case ACCESS_TUPLE:
		addr = gen_access_value(ctx, expr);
		break;
	}
	return addr;
}

static struct gen_value
gen_expr_access(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value addr = gen_expr_access_addr(ctx, expr);
	return gen_load(ctx, addr);
}

static void
gen_alloc_slice_at(struct gen_context *ctx,
		const struct expression *expr,
		struct gen_value out,
		bool expand)
{
	struct qbe_value qcap;
	if (expr->alloc.cap) {
		struct gen_value cap = gen_expr(ctx, expr->alloc.cap);
		qcap = mkqval(ctx, &cap);
	}

	struct gen_value init;
	struct qbe_value qinit;
	struct qbe_value length, initdata;
	const struct type *inittype = type_dealias(NULL, expr->alloc.init->result);
	switch (inittype->storage) {
	case STORAGE_ARRAY:
		assert(inittype->array.length != SIZE_UNDEFINED);
		length = constl(inittype->array.length);
		break;
	case STORAGE_SLICE:
		init = gen_expr(ctx, expr->alloc.init);
		qinit = mkqval(ctx, &init);
		enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
		initdata = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &initdata, load, &qinit, NULL);
		struct qbe_value offset = constl(builtin_type_size.size);
		struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &ptr, Q_ADD, &qinit, &offset, NULL);
		length = mkqtmp(ctx, ctx->arch.sz, ".%d");
		pushi(ctx->current, &length, load, &ptr, NULL);
		break;
	default: abort(); // Invariant
	}

	if (!expr->alloc.cap) {
		qcap = length;
	}

	// reused in next few blocks
	struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");

	struct qbe_statement lnotfits, lfits;
	struct qbe_value bfits = mklabel(ctx, &lfits, ".%d");
	struct qbe_value bnotfits = mklabel(ctx, &lnotfits, ".%d");
	pushi(ctx->current, &cmpres, Q_CULEL, &length, &qcap, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bfits, &bnotfits, NULL);
	push(&ctx->current->body, &lnotfits);
	gen_fixed_abort(ctx, expr->loc, ABORT_CAP_TOO_SMALL);
	push(&ctx->current->body, &lfits);

	const struct type *sltype = type_dealias(NULL, expr->result);
	struct qbe_value isize = constl(sltype->array.members->size);
	struct qbe_value size = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &size, Q_MUL, &qcap, &isize, NULL);

	struct qbe_statement lzero, lnonzero;
	struct qbe_value bzero = mklabel(ctx, &lzero, ".%d");
	struct qbe_value bnonzero = mklabel(ctx, &lnonzero, ".%d");

	struct qbe_value data = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value zero = constl(0);
	pushi(ctx->current, &data, Q_COPY, &zero, NULL);
	pushi(ctx->current, &cmpres, Q_CNEL, &size, &zero, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bnonzero, &bzero, NULL);
	push(&ctx->current->body, &lnonzero);
	pushi(ctx->current, &data, Q_CALL, &ctx->rt.malloc, &size, NULL);

	struct qbe_statement linvalid;
	struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
	pushi(ctx->current, &cmpres, Q_CNEL, &data, &zero, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bzero, &binvalid, NULL);
	push(&ctx->current->body, &linvalid);
	gen_fixed_abort(ctx, expr->loc, ABORT_ALLOC_FAILURE);
	push(&ctx->current->body, &lzero);

	struct qbe_value base = mklval(ctx, &out);
	struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value offset = constl(builtin_type_size.size);
	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, NULL, store, &data, &base, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &base, &offset, NULL);

	if (expand) {
		pushi(ctx->current, NULL, store, &qcap, &ptr, NULL);
	} else {
		pushi(ctx->current, NULL, store, &length, &ptr, NULL);
	}

	offset = constl(builtin_type_size.size * 2);
	pushi(ctx->current, &ptr, Q_ADD, &base, &offset, NULL);
	pushi(ctx->current, NULL, store, &qcap, &ptr, NULL);

	if (inittype->storage == STORAGE_ARRAY) {
		struct gen_value storage = (struct gen_value){
			.kind = GV_TEMP,
			.type = inittype,
			.name = data.name,
		};
		gen_expr_at(ctx, expr->alloc.init, storage);
	} else {
		struct qbe_value copysize = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &copysize, Q_MUL, &length, &isize, NULL);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy,
				&data, &initdata, &copysize, NULL);
	}

	if (!expand) {
		return;
	}

	struct qbe_value last = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value next = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &next, Q_MUL, &length, &isize, NULL);
	pushi(ctx->current, &next, Q_ADD, &next, &data, NULL);
	pushi(ctx->current, &last, Q_SUB, &next, &isize, NULL);
	struct qbe_value remain = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &remain, Q_SUB, &qcap, &length, NULL);
	pushi(ctx->current, &remain, Q_MUL, &remain, &isize, NULL);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &next, &last, &remain, NULL);
}

static struct gen_value
gen_expr_alloc_init_with(struct gen_context *ctx,
	const struct expression *expr, struct gen_value *out)
{
	// alloc(init) case
	assert(expr->alloc.cap == NULL);

	const struct type *objtype = type_dealias(NULL, expr->result);
	assert(objtype->storage == STORAGE_POINTER);
	objtype = objtype->pointer.referent;

	struct qbe_value sz = constl(objtype->size);
	struct gen_value result = mkgtemp(ctx, expr->result, ".%d");
	struct qbe_value qresult = mkqval(ctx, &result);
	pushi(ctx->current, &qresult, Q_CALL, &ctx->rt.malloc, &sz, NULL);

	if (!(type_dealias(NULL, expr->result)->pointer.flags & PTR_NULLABLE)) {
		struct qbe_statement linvalid, lvalid;
		struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");
		struct qbe_value zero = constl(0);
		struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
		struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");

		pushi(ctx->current, &cmpres, Q_CNEL, &qresult, &zero, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bvalid, &binvalid, NULL);
		push(&ctx->current->body, &linvalid);
		gen_fixed_abort(ctx, expr->loc, ABORT_ALLOC_FAILURE);
		push(&ctx->current->body, &lvalid);
	}

	struct gen_value object = {
		.kind = GV_TEMP,
		.type = objtype,
		.name = result.name,
	};
	gen_expr_at(ctx, expr->alloc.init, object);
	if (out) {
		gen_store(ctx, *out, result);
	}
	return result;
}

static struct gen_value
gen_expr_alloc_slice_with(struct gen_context *ctx,
	const struct expression *expr, struct gen_value *out)
{
	// alloc(init, cap | len) case
	bool expand = expr->alloc.kind == ALLOC_WITH_LEN;
	if (out) {
		gen_alloc_slice_at(ctx, expr, *out, expand);
		return gv_void;
	}
	struct gen_value temp = mkgtemp(ctx, expr->result, "object.%d");
	struct qbe_value base = mkqval(ctx, &temp);
	struct qbe_value sz = constl(expr->result->size);
	enum qbe_instr alloc = alloc_for_align(expr->result->align);
	pushprei(ctx->current, &base, alloc, &sz, NULL);
	gen_alloc_slice_at(ctx, expr, temp, expand);
	return temp;
}

static struct gen_value
gen_expr_alloc_copy_with(struct gen_context *ctx,
	const struct expression *expr, struct gen_value *out)
{
	// alloc(init...) case
	assert(expr->alloc.cap == NULL);

	struct gen_value ret = gv_void;
	if (out == NULL) {
		ret = mkgtemp(ctx, expr->result, "object.%d");
		out = &ret;

		struct qbe_value base = mkqval(ctx, out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
	}

	enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);

	struct gen_value src = gen_expr(ctx, expr->alloc.init);
	struct qbe_value dbase = mkcopy(ctx, out, ".%d");
	struct qbe_value offs = constl(builtin_type_size.size);

	const struct type *initres = type_dealias(NULL, expr->alloc.init->result);
	struct qbe_value srcdata;
	struct qbe_value length;
	if (initres->storage == STORAGE_SLICE) {
		assert(initres->array.length == SIZE_UNDEFINED);
		srcdata = mkqtmp(ctx, ctx->arch.sz, ".%d");
		struct qbe_value sbase = mkcopy(ctx, &src, ".%d");
		pushi(ctx->current, &srcdata, load, &sbase, NULL);
		pushi(ctx->current, &sbase, Q_ADD, &sbase, &offs, NULL);
		length = mkqtmp(ctx, ctx->arch.sz, ".%d");
		pushi(ctx->current, &length, load, &sbase, NULL);
	} else if (initres->storage == STORAGE_ARRAY) {
		assert(initres->array.length != SIZE_UNDEFINED);
		srcdata = mkcopy(ctx, &src, ".%d"); // TODO: object.%d
		length = constl(initres->array.length);
	} else {
		abort();
	}

	struct qbe_statement linvalid, lvalid, lalloc, lcopy;
	struct qbe_value balloc = mklabel(ctx, &lalloc, ".%d");
	struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
	struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");
	struct qbe_value bcopy = mklabel(ctx, &lcopy, ".%d");

	struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value newdata = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value zero = constl(0);
	pushi(ctx->current, &newdata, Q_COPY, &zero, NULL);
	pushi(ctx->current, &cmpres, Q_CNEL, &length, &zero, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &cmpres, &balloc, &bvalid, NULL);
	push(&ctx->current->body, &lalloc);

	const struct type *result = type_dealias(NULL, expr->result);
	assert(result->storage == STORAGE_SLICE);
	struct qbe_value sz = mkqtmp(ctx, ctx->arch.sz, ".%d");
	struct qbe_value membsz = constl(result->array.members->size);
	pushi(ctx->current, &sz, Q_MUL, &membsz, &length, NULL);

	pushi(ctx->current, &newdata, Q_CALL, &ctx->rt.malloc, &sz, NULL);
	pushi(ctx->current, &cmpres, Q_CNEL, &newdata, &zero, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bcopy, &binvalid, NULL);

	push(&ctx->current->body, &linvalid);
	gen_fixed_abort(ctx, expr->loc, ABORT_ALLOC_FAILURE);

	push(&ctx->current->body, &lcopy);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &newdata, &srcdata, &sz, NULL);

	push(&ctx->current->body, &lvalid);
	pushi(ctx->current, NULL, store, &newdata, &dbase, NULL);
	pushi(ctx->current, &dbase, Q_ADD, &dbase, &offs, NULL);
	pushi(ctx->current, NULL, store, &length, &dbase, NULL);
	pushi(ctx->current, &dbase, Q_ADD, &dbase, &offs, NULL);
	pushi(ctx->current, NULL, store, &length, &dbase, NULL);

	return ret;
}

static struct gen_value
gen_expr_alloc_with(struct gen_context *ctx,
	const struct expression *expr, struct gen_value *out)
{
	switch (expr->alloc.kind) {
	case ALLOC_OBJECT:
		return gen_expr_alloc_init_with(ctx, expr, out);
	case ALLOC_WITH_CAP:
	case ALLOC_WITH_LEN:
		return gen_expr_alloc_slice_with(ctx, expr, out);
	case ALLOC_COPY:
		return gen_expr_alloc_copy_with(ctx, expr, out);
	}
	abort(); // Unreachable
}

static struct gen_value
gen_expr_append(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value slice = gen_expr(ctx, expr->append.object);
	slice = gen_autoderef(ctx, slice);
	struct qbe_value qslice = mkqval(ctx, &slice);

	struct qbe_value lenptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value prevlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
	struct qbe_value offs = constl(builtin_type_size.size);
	pushi(ctx->current, &lenptr, Q_ADD, &qslice, &offs, NULL);
	enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, &prevlen, load, &lenptr, NULL);

	struct qbe_value appendlen;
	const struct type *valtype = type_dealias(NULL, expr->append.value->result);
	if (expr->append.length != NULL) {
		struct gen_value length = gen_expr(ctx, expr->append.length);
		appendlen = mkqval(ctx, &length);
		assert(valtype->storage == STORAGE_ARRAY && valtype->array.expandable);
	} else if (!expr->append.is_multi) {
		appendlen = constl(1);
	}

	struct gen_value value;
	struct qbe_value qvalue;
	if (!expr->append.is_multi || valtype->storage != STORAGE_ARRAY) {
		// We use gen_expr_at for the array case to avoid a copy
		value = gen_expr(ctx, expr->append.value);
		qvalue = mkqval(ctx, &value);
	}

	if (expr->append.is_multi) {
		if (valtype->storage == STORAGE_ARRAY) {
			assert(valtype->array.length != SIZE_UNDEFINED);
			appendlen = constl(valtype->array.length);
		} else {
			appendlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
			struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
			offs = constl(builtin_type_size.size);
			pushi(ctx->current, &ptr, Q_ADD, &qvalue, &offs, NULL);
			pushi(ctx->current, &appendlen, load, &ptr, NULL);
		}
	}

	struct qbe_value newlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &newlen, Q_ADD, &prevlen, &appendlen, NULL);
	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, NULL, store, &newlen, &lenptr, NULL);

	struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	const struct type *mtype = type_dealias(NULL, slice.type)->array.members;
	struct qbe_value membsz = constl(mtype->size);
	if (!expr->append.is_static) {
		struct qbe_value lval = mklval(ctx, &slice);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.ensure, &lval, &membsz, NULL);
	} else {
		offs = constl(builtin_type_size.size * 2);
		pushi(ctx->current, &ptr, Q_ADD, &qslice, &offs, NULL);
		struct qbe_value cap = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &cap, load, &ptr, NULL);

		struct qbe_statement lvalid, linvalid;
		struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");
		struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
		struct qbe_value valid = mkqtmp(ctx, &qbe_word, ".%d");
		pushi(ctx->current, &valid, Q_CULEL, &newlen, &cap, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &valid, &bvalid, &binvalid, NULL);

		push(&ctx->current->body, &linvalid);
		gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
		push(&ctx->current->body, &lvalid);
	}

	offs = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &ptr, load, &qslice, NULL);
	pushi(ctx->current, &offs, Q_MUL, &prevlen, &membsz, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &ptr, &offs, NULL);

	struct gen_value item = {
		.kind = GV_TEMP,
		.type = mtype,
		.name = ptr.name,
	};
	if (expr->append.is_multi && valtype->storage == STORAGE_ARRAY) {
		item.type = valtype;
		gen_expr_at(ctx, expr->append.value, item);
	} else if (expr->append.is_multi && valtype->storage == STORAGE_SLICE) {
		struct qbe_value qsrc = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		struct qbe_value sz = mkqtmp(ctx, ctx->arch.sz, ".%d");
		pushi(ctx->current, &sz, Q_MUL, &appendlen, &membsz, NULL);
		pushi(ctx->current, &qsrc, load, &qvalue, NULL);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove, &ptr, &qsrc, &sz, NULL);
	} else if (expr->append.length != NULL) {
		// XXX: This could be made more efficient for some cases if
		// check could determine the length at compile time and lower it
		// to a fixed-length array type
		assert(valtype->storage == STORAGE_ARRAY);
		item.type = valtype;
		gen_expr_at(ctx, expr->append.value, item);

		assert(valtype->array.length != SIZE_UNDEFINED);
		struct qbe_value next = mkqtmp(ctx, ctx->arch.ptr, "next.%d");
		struct qbe_value last = mkqtmp(ctx, ctx->arch.ptr, "last.%d");
		struct qbe_value arlen = constl(valtype->array.length * mtype->size);
		pushi(ctx->current, &next, Q_ADD, &ptr, &arlen, NULL);
		arlen = constl((valtype->array.length - 1) * mtype->size);
		pushi(ctx->current, &last, Q_ADD, &ptr, &arlen, NULL);

		struct qbe_value remain = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		struct qbe_value one = constl(1);
		pushi(ctx->current, &remain, Q_SUB, &appendlen, &one, NULL);
		pushi(ctx->current, &remain, Q_MUL, &remain, &membsz, NULL);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &next, &last, &remain, NULL);
	} else {
		gen_store(ctx, item, value);
	}

	return gv_void;
}

static struct gen_value
gen_expr_assert(struct gen_context *ctx, const struct expression *expr)
{
	struct qbe_statement failedl, passedl;
	if (expr->assert.cond) {
		struct qbe_value bfailed = mklabel(ctx, &failedl, "failed.%d");
		struct qbe_value bpassed = mklabel(ctx, &passedl, "passed.%d");
		struct gen_value cond = gen_expr(ctx, expr->assert.cond);
		struct qbe_value qcond = mkqval(ctx, &cond);
		pushi(ctx->current, NULL, Q_JNZ, &qcond, &bpassed, &bfailed, NULL);
		push(&ctx->current->body, &failedl);
	}

	struct gen_value msg, gloc;
	if (expr->assert.message) {
		struct expression eloc;
		mkstrconst(&eloc, "%s:%d:%d", sources[expr->loc.file],
			expr->loc.lineno, expr->loc.colno);
		gloc = gen_expr(ctx, &eloc);
		msg = gen_expr(ctx, expr->assert.message);
	}

	if (expr->assert.message) {
		for (struct gen_scope *scope = ctx->scope;
				scope; scope = scope->parent) {
			gen_defers(ctx, scope);
			if (scope->scope->class == SCOPE_DEFER) {
				break;
			}
		}
		struct qbe_value qmsg = mkqval(ctx, &msg), qloc = mkqval(ctx, &gloc);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.abort, &qloc, &qmsg, NULL);
		pushi(ctx->current, NULL, Q_HLT, NULL);
	} else {
		gen_fixed_abort(ctx, expr->loc, expr->assert.fixed_reason);
	}

	if (expr->assert.cond) {
		push(&ctx->current->body, &passedl);
	}

	return gv_void;
}

static struct gen_value
gen_expr_assign_slice_expandable(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *sltype = expr->assign.object->result->array.members;
	const struct qbe_type *slqtype = qtype_lookup(ctx, sltype, false);

	struct gen_value obj = gen_expr(ctx, expr->assign.object);
	struct qbe_value qobj = mkqval(ctx, &obj);

	// get the length of the copy
	struct qbe_value step = constl(ctx->arch.ptr->size);
	struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value olen = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &ptr, Q_ADD, &qobj, &step, NULL);
	pushi(ctx->current, &olen, Q_LOADL, &ptr, NULL);
	
	// check if there is anything to do
	struct qbe_statement lzero, lnonzero;
	struct qbe_value bzero = mklabel(ctx, &lzero, ".%d");
	struct qbe_value bnonzero = mklabel(ctx, &lnonzero, ".%d");
	
	struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value zero = constl(0);
	pushi(ctx->current, &cmpres, Q_CNEL, &olen, &zero, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bnonzero, &bzero, NULL);
	push(&ctx->current->body, &lnonzero);

	// get the destination
	struct qbe_value odata = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &odata, Q_LOADL, &qobj, NULL);

	// get the source
	struct gen_value val = gen_expr(ctx, expr->assign.value);
	struct qbe_value qval = mkqval(ctx, &val);
	struct qbe_value vdata = mkqtmp(ctx, slqtype, ".%d");
	enum qbe_instr load = load_for_type(ctx, sltype);
	pushi(ctx->current, &vdata, load, &qval, NULL);

	// copy the first item
	enum qbe_instr store = store_for_type(ctx, sltype);
	pushi(ctx->current, NULL, store, &vdata, &odata, NULL);

	// perform the copy minus the first element
	struct qbe_value isize = constl(sltype->size);
	struct qbe_value next = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &next, Q_ADD, &odata, &isize, NULL);
	struct qbe_value one = constl(1);
	pushi(ctx->current, &olen, Q_SUB, &olen, &one, NULL);
	pushi(ctx->current, &olen, Q_MUL, &olen, &isize, NULL);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &next, &odata, &olen, NULL);
	
	push(&ctx->current->body, &lzero);
	
	return gv_void;
}

static struct gen_value
gen_expr_assign_slice(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *vtype = type_dealias(NULL, expr->assign.value->result);
	if (vtype->storage == STORAGE_ARRAY && vtype->array.expandable) {
		return gen_expr_assign_slice_expandable(ctx, expr);
	}

	struct gen_value obj = gen_expr(ctx, expr->assign.object);
	struct gen_value val = gen_expr(ctx, expr->assign.value);
	struct qbe_value qobj = mkqval(ctx, &obj);
	struct qbe_value qval = mkqval(ctx, &val);
	struct qbe_value step = constl(ctx->arch.ptr->size);

	struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value olen = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value vlen = mkqtmp(ctx, ctx->arch.ptr, ".%d");

	pushi(ctx->current, &ptr, Q_ADD, &qobj, &step, NULL);
	pushi(ctx->current, &olen, Q_LOADL, &ptr, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &qval, &step, NULL);
	pushi(ctx->current, &vlen, Q_LOADL, &ptr, NULL);

	struct qbe_statement linvalid, lvalid;
	struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
	struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");
	struct qbe_value tmp = mkqtmp(ctx, &qbe_long, ".%d");
	pushi(ctx->current, &tmp, Q_CEQL, &olen, &vlen, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &tmp, &bvalid, &binvalid, NULL);
	push(&ctx->current->body, &linvalid);
	gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
	push(&ctx->current->body, &lvalid);

	struct qbe_value optr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value vptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &optr, Q_LOADL, &qobj, NULL);
	pushi(ctx->current, &vptr, Q_LOADL, &qval, NULL);
	tmp = constl(expr->assign.object->result->array.members->size);
	pushi(ctx->current, &olen, Q_MUL, &olen, &tmp, NULL);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove, &optr, &vptr, &olen, NULL);

	return gv_void;
}

static struct qbe_value
extend(struct gen_context *ctx, struct qbe_value v, const struct type *type)
{
	enum qbe_instr op;
	switch (type->size) {
	case 1:
		op = type_is_signed(NULL, type) ? Q_EXTSB : Q_EXTUB;
		break;
	case 2:
		op = type_is_signed(NULL, type) ? Q_EXTSH : Q_EXTUH;
		break;
	default:
		return v;
	}

	struct qbe_value temp = mkqtmp(ctx, &qbe_word, "ext.%d");
	pushi(ctx->current, &temp, op, &v, NULL);
	return temp;
}

bool bin_extend[BIN_LAST + 1][2] = {
	[BIN_BAND] = { false, false },
	[BIN_BOR] = { false, false },
	[BIN_DIV] = { true, true },
	[BIN_GREATER] = { true, true },
	[BIN_GREATEREQ] = { true, true },
	[BIN_LAND] = { true, true },
	[BIN_LEQUAL] = { true, true },
	[BIN_LESS] = { true, true },
	[BIN_LESSEQ] = { true, true },
	[BIN_LOR] = { true, true },
	[BIN_LSHIFT] = { false, true },
	[BIN_LXOR] = { true, true },
	[BIN_MINUS] = { true, false },
	[BIN_MODULO] = { true, true },
	[BIN_NEQUAL] = { true, true },
	[BIN_PLUS] = { false, false },
	[BIN_RSHIFT] = { true, true },
	[BIN_TIMES] = { false, false },
	[BIN_BXOR] = { false, false },
};

static struct gen_value
gen_expr_assign(struct gen_context *ctx, const struct expression *expr)
{
	struct expression *object = expr->assign.object;
	struct expression *value = expr->assign.value;
	if (object->type == EXPR_SLICE) {
		return gen_expr_assign_slice(ctx, expr);
	}

	struct gen_value obj;
	switch (object->type) {
	case EXPR_ACCESS:
		obj = gen_expr_access_addr(ctx, object);
		break;
	case EXPR_UNARITHM:
		assert(object->unarithm.op == UN_DEREF); // Invariant
		obj = gen_expr(ctx, object->unarithm.operand);
		assert(type_dealias(NULL, obj.type)->storage == STORAGE_POINTER);
		obj.type = type_dealias(NULL, obj.type)->pointer.referent;
		break;
	default:
		abort(); // Invariant
	}
	if (expr->assign.op == BIN_LEQUAL) {
		gen_store(ctx, obj, gen_expr(ctx, value));
	} else if (expr->assign.op == BIN_LAND || expr->assign.op == BIN_LOR) {
		struct qbe_statement lrval, lshort;
		struct qbe_value brval = mklabel(ctx, &lrval, ".%d");
		struct qbe_value bshort = mklabel(ctx, &lshort, ".%d");
		struct gen_value load = gen_load(ctx, obj);
		struct qbe_value qload = mkqval(ctx, &load);
		if (expr->binarithm.op == BIN_LAND) {
			pushi(ctx->current, NULL, Q_JNZ, &qload, &brval,
				&bshort, NULL);
		} else {
			pushi(ctx->current, NULL, Q_JNZ, &qload, &bshort,
				&brval, NULL);
		}
		push(&ctx->current->body, &lrval);
		gen_expr_at(ctx, value, obj);
		if (!expr->binarithm.rvalue->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &bshort, NULL);
		}
		push(&ctx->current->body, &lshort);
	} else {
		struct gen_value lvalue = gen_load(ctx, obj);
		struct gen_value rvalue = gen_expr(ctx, value);
		struct qbe_value qlval = mkqval(ctx, &lvalue);
		struct qbe_value ilval = qlval;
		struct qbe_value qrval = mkqval(ctx, &rvalue);
		enum qbe_instr instr = binarithm_for_op(ctx,
			expr->assign.op, lvalue.type);
		if (bin_extend[expr->assign.op][0]) {
			ilval = extend(ctx, ilval, lvalue.type);
		}
		if (bin_extend[expr->assign.op][1]) {
			qrval = extend(ctx, qrval, rvalue.type);
		}
		pushi(ctx->current, &qlval, instr, &ilval, &qrval, NULL);
		gen_store(ctx, obj, lvalue);
	}

	return gv_void;
}

static struct gen_value
gen_expr_binarithm(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *ltype = type_dealias(NULL, expr->binarithm.lvalue->result);
	const struct type *rtype = type_dealias(NULL, expr->binarithm.rvalue->result);
	struct gen_value result = mkgtemp(ctx, expr->result, ".%d");
	struct qbe_value qresult = mkqval(ctx, &result);

	if (expr->binarithm.op == BIN_LAND || expr->binarithm.op == BIN_LOR) {
		struct qbe_statement lrval, lshort;
		struct qbe_value brval = mklabel(ctx, &lrval, ".%d");
		struct qbe_value bshort = mklabel(ctx, &lshort, ".%d");
		struct gen_value lval = gen_expr(ctx, expr->binarithm.lvalue);
		struct qbe_value qlval = mkqval(ctx, &lval);
		pushi(ctx->current, &qresult, Q_COPY, &qlval, NULL);
		if (expr->binarithm.op == BIN_LAND) {
			pushi(ctx->current, NULL, Q_JNZ, &qresult, &brval,
				&bshort, NULL);
		} else {
			pushi(ctx->current, NULL, Q_JNZ, &qresult, &bshort,
				&brval, NULL);
		}
		push(&ctx->current->body, &lrval);
		struct gen_value rval = gen_expr(ctx, expr->binarithm.rvalue);
		struct qbe_value qrval = mkqval(ctx, &rval);
		pushi(ctx->current, &qresult, Q_COPY, &qrval, NULL);
		if (!expr->binarithm.rvalue->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &bshort, NULL);
		}
		push(&ctx->current->body, &lshort);
		return result;
	}

	struct gen_value lvalue = gen_expr(ctx, expr->binarithm.lvalue);
	struct gen_value rvalue = gen_expr(ctx, expr->binarithm.rvalue);
	struct qbe_value qlval = mkqval(ctx, &lvalue);
	struct qbe_value qrval = mkqval(ctx, &rvalue);

	if (bin_extend[expr->assign.op][0]) {
		qlval = extend(ctx, qlval, ltype);
	}
	if (bin_extend[expr->assign.op][1]) {
		qrval = extend(ctx, qrval, rtype);
	}

	assert((ltype->storage == STORAGE_STRING) == (rtype->storage == STORAGE_STRING));
	if (ltype->storage == STORAGE_STRING) {
		pushi(ctx->current, &qresult, Q_CALL,
			&ctx->rt.strcmp, &qlval, &qrval, NULL);
		if (expr->binarithm.op == BIN_NEQUAL) {
			struct qbe_value one = constl(1);
			pushi(ctx->current, &qresult, Q_XOR, &qresult, &one, NULL);
		} else {
			assert(expr->binarithm.op == BIN_LEQUAL);
		}
		return result;
	}
	enum qbe_instr instr = binarithm_for_op(ctx, expr->binarithm.op,
		expr->binarithm.lvalue->result);
	pushi(ctx->current, &qresult, instr, &qlval, &qrval, NULL);
	return result;
}

static void
gen_expr_binding_unpack_static(struct gen_context *ctx,
	const struct expression_binding *binding)
{
	assert(binding->object == NULL);

	struct tuple_constant *tupleconst =
		binding->initializer->constant.tuple;

	for (const struct binding_unpack *unpack = binding->unpack;
			unpack; unpack = unpack->next) {
		if (unpack->object == NULL) {
			goto done;
		}
		assert(unpack->object->otype == O_DECL);

		struct declaration decl = {
			.decl_type = DECL_GLOBAL,
			.ident = unpack->object->ident,
			.global = {
				.type = unpack->object->type,
				.value = tupleconst->value,
			},
		};
		gen_global_decl(ctx, &decl);

done:
		tupleconst = tupleconst->next;
	}
}

static void
gen_expr_binding_unpack(struct gen_context *ctx,
	const struct expression_binding *binding)
{
	assert(binding->object == NULL);

	const struct type *type = binding->initializer->result;
	char *tuple_name = gen_name(&ctx->id, "tupleunpack.%d");
	struct gen_value tuple_gv = {
		.kind = GV_TEMP,
		.type = type,
		.name = tuple_name,
	};
	struct qbe_value tuple_qv = mklval(ctx, &tuple_gv);
	struct qbe_value sz = constl(type->size);
	enum qbe_instr alloc = alloc_for_align(type->align);
	pushprei(ctx->current, &tuple_qv, alloc, &sz, NULL);

	gen_expr_at(ctx, binding->initializer, tuple_gv);

	for (const struct binding_unpack *unpack = binding->unpack;
			unpack; unpack = unpack->next) {
		if (unpack->object == NULL) {
			continue;
		}
		assert(unpack->object->otype != O_DECL);

		const struct type *type = unpack->object->type;
		struct gen_value item_gv = {
			.kind = GV_TEMP,
			.type = type,
			.name = gen_name(&ctx->id, "binding.%d"),
		};
		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value = item_gv;
		gb->object = unpack->object;
		gb->next = ctx->bindings;
		ctx->bindings = gb;
		struct qbe_value item_qv = mklval(ctx, &gb->value);
		struct qbe_value offs = constl(unpack->offset);
		pushprei(ctx->current, &item_qv, Q_ADD, &tuple_qv, &offs, NULL);
	}
}

static struct gen_value
gen_expr_binding(struct gen_context *ctx, const struct expression *expr)
{
	for (const struct expression_binding *binding = &expr->binding;
			binding; binding = binding->next) {
		if (binding->unpack) {
			if (binding->unpack->object->otype == O_DECL) {
				gen_expr_binding_unpack_static(ctx, binding);
			} else {
				gen_expr_binding_unpack(ctx, binding);
			}
			continue;
		}

		if (binding->object->otype == O_DECL) {
			// static binding
			struct declaration decl = {
				.decl_type = DECL_GLOBAL,
				.ident = binding->object->ident,
				.global = {
					.type = binding->object->type,
					.value = binding->initializer,
				},
			};
			gen_global_decl(ctx, &decl);
			continue;
		}

		const struct type *type = binding->object->type;
		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value.kind = GV_TEMP;
		gb->value.type = type;
		gb->value.name = gen_name(&ctx->id, "binding.%d");
		gb->object = binding->object;
		gb->next = ctx->bindings;
		ctx->bindings = gb;

		struct qbe_value qv = mklval(ctx, &gb->value);
		struct qbe_value sz = constl(type->size);
		enum qbe_instr alloc = alloc_for_align(type->align);
		pushprei(ctx->current, &qv, alloc, &sz, NULL);
		gen_expr_at(ctx, binding->initializer, gb->value);
	}
	return gv_void;
}

static struct gen_value
gen_expr_control(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_scope *scope = gen_scope_lookup(ctx, expr->control.scope);

	if (expr->control.value) {
		struct gen_value result = gen_expr_with(ctx,
			expr->control.value, scope->out);
		branch_copyresult(ctx, result,
			scope->result, scope->out);
	}

	struct gen_scope *deferred = ctx->scope;
	while (deferred != NULL) {
		gen_defers(ctx, deferred);
		if (deferred == scope) {
			break;
		}
		deferred = deferred->parent;
	}

	switch (expr->type) {
	case EXPR_BREAK:
		assert(scope->scope->class == SCOPE_LOOP);
		pushi(ctx->current, NULL, Q_JMP, scope->end, NULL);
		break;
	case EXPR_CONTINUE:
		assert(scope->scope->class == SCOPE_LOOP);
		pushi(ctx->current, NULL, Q_JMP, scope->after, NULL);
		break;
	case EXPR_YIELD:
		assert(scope->scope->class == SCOPE_COMPOUND);
		pushi(ctx->current, NULL, Q_JMP, scope->end, NULL);
		break;
	default: abort(); // Invariant
	}
	return gv_void;
}

static struct gen_value
gen_expr_call(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value lvalue = gen_expr(ctx, expr->call.lvalue);
	lvalue = gen_autoderef(ctx, lvalue);

	const struct type *rtype = type_dealias(NULL, lvalue.type);
	assert(rtype->storage == STORAGE_FUNCTION);

	if (rtype->func.flags & FN_NORETURN) {
		for (struct gen_scope *scope = ctx->scope; scope;
				scope = scope->parent) {
			gen_defers(ctx, scope);
			if (scope->scope->class == SCOPE_DEFER) {
				break;
			}
		}
	}

	struct qbe_statement call = {
		.type = Q_INSTR,
		.instr = Q_CALL,
	};
	struct gen_value rval = gv_void;
	if (type_dealias(NULL, rtype->func.result)->storage != STORAGE_VOID) {
		rval = mkgtemp(ctx, rtype->func.result, "returns.%d");
		call.out = xcalloc(1, sizeof(struct qbe_value));
		*call.out = mkqval(ctx, &rval);
		call.out->type = qtype_lookup(ctx, rtype->func.result, false);
	}

	bool cvar = false;
	struct type_func_param *param = rtype->func.params;
	struct qbe_arguments *args, **next = &call.args;
	args = *next = xcalloc(1, sizeof(struct qbe_arguments));
	args->value = mkqval(ctx, &lvalue);
	next = &args->next;
	for (struct call_argument *carg = expr->call.args;
			carg; carg = carg->next) {
		args = *next = xcalloc(1, sizeof(struct qbe_arguments));
		struct gen_value arg = gen_expr(ctx, carg->value);
		args->value = mkqval(ctx, &arg);
		args->value.type = qtype_lookup(ctx, carg->value->result, false);
		next = &args->next;
		if (param) {
			param = param->next;
		}
		if (!param && !cvar && rtype->func.variadism == VARIADISM_C) {
			cvar = true;
			args = *next = xcalloc(1, sizeof(struct qbe_arguments));
			args->value.kind = QV_VARIADIC;
			next = &args->next;
		}
	}
	push(&ctx->current->body, &call);

	return rval;
}

static struct gen_value gen_expr_cast(struct gen_context *ctx,
		const struct expression *expr);

static struct gen_value gen_subset_match_tests(struct gen_context *ctx,
	struct qbe_value bmatch, struct qbe_value bnext,
	struct qbe_value tag, const struct type *type);

static struct gen_value gen_nested_match_tests(struct gen_context *ctx,
		struct gen_value object, struct qbe_value bmatch,
		struct qbe_value bnext, struct qbe_value tag,
		const struct type *type);

static struct gen_value
gen_type_assertion_or_test(struct gen_context *ctx, const struct expression *expr,
		struct gen_value base)
{
	assert(expr->cast.kind == C_TEST || expr->cast.kind == C_ASSERTION);
	const struct type *want = expr->cast.secondary;
	struct qbe_value tag = mkqtmp(ctx,
		qtype_lookup(ctx, &builtin_type_uint, false), ".%d");
	enum qbe_instr load = load_for_type(ctx, &builtin_type_uint);
	struct qbe_value qbase = mkqval(ctx, &base);
	pushi(ctx->current, &tag, load, &qbase, NULL);

	struct qbe_statement failedl, passedl;
	struct qbe_value bfailed, bpassed = mklabel(ctx, &passedl, "passed.%d");
	if (expr->cast.kind == C_ASSERTION) {
		bfailed = mklabel(ctx, &failedl, "failed.%d");
	} else {
		bfailed = bpassed;
	}
	struct gen_value result = {0};
	if (tagged_select_subtype(NULL, expr->cast.value->result, want, true)) {
		result = gen_nested_match_tests(ctx, base, bpassed,
				bfailed, tag, want);
	} else if (tagged_subset_compat(NULL, expr->cast.value->result, want)) {
		result = gen_subset_match_tests(ctx, bpassed, bfailed, tag,
				type_dealias(NULL, want));
	} else {
		abort();
	}

	if (expr->cast.kind == C_ASSERTION) {
		push(&ctx->current->body, &failedl);
		gen_fixed_abort(ctx, expr->loc, ABORT_TYPE_ASSERTION);
	}
	push(&ctx->current->body, &passedl);
	return result;
}

static void
gen_expr_cast_slice_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	const struct type *to = expr->result,
		*from = type_dealias(NULL, expr->cast.value->result);
	if (from->storage == STORAGE_POINTER) {
		from = type_dealias(NULL, from->pointer.referent);
	}
	assert(from->storage == STORAGE_ARRAY);
	assert(from->array.length != SIZE_UNDEFINED);

	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	struct qbe_value base = mklval(ctx, &out);
	struct qbe_value sz = constl(to->size);
	if (from->array.length == 0) {
		struct qbe_value tmp = constl(0);
		pushi(ctx->current, NULL, store, &tmp, &base, NULL);
	} else {
		struct gen_value value = gen_expr(ctx, expr->cast.value);
		struct qbe_value qvalue = mkqval(ctx, &value);
		pushi(ctx->current, NULL, store, &qvalue, &base, NULL);
	}
	struct qbe_value qptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	sz = constl(builtin_type_size.size);
	struct qbe_value ln = constl(from->array.length);
	pushi(ctx->current, &qptr, Q_ADD, &base, &sz, NULL);
	pushi(ctx->current, NULL, store, &ln, &qptr, NULL);
	pushi(ctx->current, &qptr, Q_ADD, &qptr, &sz, NULL);
	pushi(ctx->current, NULL, store, &ln, &qptr, NULL);
}

// Returns true if object's storage can be interpreted as want.
static bool
tagged_align_compat(const struct type *object, const struct type *want)
{
	assert(type_dealias(NULL, object)->storage == STORAGE_TAGGED);
	assert(type_dealias(NULL, want)->storage == STORAGE_TAGGED);
	return object->align == want->align
		|| want->size == builtin_type_uint.size;
}

static void
gen_expr_cast_tagged_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	assert(expr->type == EXPR_CAST);
	const struct type *to = expr->result, *from = expr->cast.value->result;
	const struct type *subtype = tagged_select_subtype(NULL, to, from, true);

	if (!subtype && tagged_align_compat(from, to)) {
		// Case 1: from is a union whose members are a subset or
		// superset of to, and the alignment matches, so we can just
		// interpret values of type 'from' as if it were of type 'to'
		struct gen_value out2 = out;
		out2.type = from;
		gen_expr_at(ctx, expr->cast.value, out2);
		if (expr->cast.kind == C_ASSERTION) {
			gen_type_assertion_or_test(ctx, expr, out2);
		}
	} else if (!subtype) {
		// Case 2: like case 1, but with an alignment mismatch; more
		// work is required.
		struct gen_value value = gen_expr(ctx, expr->cast.value);
		struct qbe_value qval = mkqval(ctx, &value);
		if (expr->cast.kind == C_ASSERTION) {
			gen_type_assertion_or_test(ctx, expr, value);
		}
		struct qbe_value qout = mkqval(ctx, &out);
		struct qbe_value tag = mkqtmp(ctx,
			qtype_lookup(ctx, &builtin_type_uint, false), "tag.%d");
		enum qbe_instr load = load_for_type(ctx, &builtin_type_uint);
		enum qbe_instr store = store_for_type(ctx, &builtin_type_uint);
		pushi(ctx->current, &tag, load, &qval, NULL);
		pushi(ctx->current, NULL, store, &tag, &qout, NULL);
		if (to->size == builtin_type_uint.size ||
				from->size == builtin_type_uint.size) {
			// No data area to copy
			return;
		}

		subtype = tagged_subset_compat(NULL, to, from) ? from : to;
		const struct type *innertype = type_store_tagged_to_union(
				ctx->store, type_dealias(NULL, subtype));
		struct gen_value iout = mkgtemp(ctx, innertype, ".%d");
		struct gen_value ival = mkgtemp(ctx, innertype, ".%d");
		struct qbe_value qiout = mkqval(ctx, &iout);
		struct qbe_value qival = mkqval(ctx, &ival);
		struct qbe_value offs = constl(to->align);
		pushi(ctx->current, &qiout, Q_ADD, &qout, &offs, NULL);
		offs = constl(from->align);
		pushi(ctx->current, &qival, Q_ADD, &qval, &offs, NULL);
		gen_copy_aligned(ctx, iout, ival);
	} else {
		// Case 3: from is a member of to
		assert(subtype == from); // Lowered by check
		struct qbe_value qout = mkqval(ctx, &out);
		struct qbe_value id = constw(subtype->id);
		enum qbe_instr store = store_for_type(ctx, &builtin_type_uint);
		pushi(ctx->current, NULL, store, &id, &qout, NULL);
		if (subtype->size == 0) {
			gen_expr(ctx, expr->cast.value); // side-effects
			return;
		}

		struct gen_value storage = mkgtemp(ctx, subtype, ".%d");
		struct qbe_value qstor = mklval(ctx, &storage);
		struct qbe_value offs = constl(to->align);
		pushi(ctx->current, &qstor, Q_ADD, &qout, &offs, NULL);
		gen_expr_at(ctx, expr->cast.value, storage);
	}
}

static bool
cast_prefers_at(const struct expression *expr)
{
	const struct type *to = expr->result, *from = expr->cast.value->result;
	if (expr->cast.kind == C_TEST) {
		return false;
	}
	// tagged => *; subtype compatible
	if (type_dealias(NULL, from)->storage == STORAGE_TAGGED
			&& tagged_select_subtype(NULL, from, to, true)) {
		return false;
	}
	// * => tagged
	if (type_dealias(NULL, to)->storage == STORAGE_TAGGED) {
		return true;
	}
	// array => array
	if (type_dealias(NULL, to)->storage == STORAGE_ARRAY
			&& type_dealias(NULL, from)->storage == STORAGE_ARRAY) {
		return true;
	}
	// array => slice
	if (type_dealias(NULL, to)->storage == STORAGE_SLICE) {
		switch (type_dealias(NULL, from)->storage) {
		case STORAGE_ARRAY:
			return true;
		case STORAGE_POINTER:
			from = type_dealias(NULL, from)->pointer.referent;
			return type_dealias(NULL, from)->storage == STORAGE_ARRAY;
		default:
			return false;
		}
	}
	return false;
}

static void
gen_expr_cast_array_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	const struct type *typeout = type_dealias(NULL, expr->result);
	const struct type *typein = type_dealias(NULL, expr->cast.value->result);
	gen_expr_at(ctx, expr->cast.value, out);
	if (!typein->array.expandable) {
		return;
	}

	assert(typein->array.length != SIZE_UNDEFINED
			&& typeout->array.length != SIZE_UNDEFINED);
	assert(typeout->array.length >= typein->array.length);

	const struct type *membtype = typein->array.members;
	size_t remain = typeout->array.length - typein->array.length;

	struct qbe_value base = mkqval(ctx, &out);
	struct qbe_value offs = constl((typein->array.length - 1) * membtype->size);
	struct gen_value next = mkgtemp(ctx, membtype, ".%d");
	struct qbe_value ptr = mklval(ctx, &next);
	struct gen_value item = mkgtemp(ctx, membtype, "item.%d");
	struct qbe_value qitem = mklval(ctx, &item);
	pushi(ctx->current, &qitem, Q_ADD, &base, &offs, NULL);

	if (remain * membtype->size <= 128) {
		struct gen_value last = gen_load(ctx, item);
		for (size_t n = typein->array.length; n < typeout->array.length; ++n) {
			struct qbe_value offs = constl(n * membtype->size);
			pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
			gen_store(ctx, next, last);
		}
		return;
	}

	offs = constl(typein->array.length * membtype->size);
	pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);

	struct qbe_value dtemp = mklval(ctx, &next);
	struct qbe_value stemp = mklval(ctx, &item);
	struct qbe_value sz = constl(remain * membtype->size);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &dtemp, &stemp, &sz, NULL);
}

static void
gen_expr_cast_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	if (!cast_prefers_at(expr)) {
		struct gen_value result = gen_expr_cast(ctx, expr);
		if (!expr->terminates) {
			gen_store(ctx, out, result);
		}
		return;
	}

	if (expr->cast.lowered) {
		pushc(ctx->current, "gen lowered cast");
	}

	const struct type *to = expr->result;
	switch (type_dealias(NULL, to)->storage) {
	case STORAGE_SLICE:
		gen_expr_cast_slice_at(ctx, expr, out);
		break;
	case STORAGE_TAGGED:
		gen_expr_cast_tagged_at(ctx, expr, out);
		break;
	case STORAGE_ARRAY:
		gen_expr_cast_array_at(ctx, expr, out);
		break;
	default: abort(); // Invariant
	}
}

static struct qbe_value nested_tagged_offset(const struct type *tu,
		const struct type *targed);

static struct gen_value
gen_expr_cast(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *to = expr->cast.secondary,
		*from = expr->cast.value->result;
	if (expr->cast.kind != C_CAST) {
		bool is_valid_tagged, is_valid_pointer;
		is_valid_tagged = type_dealias(NULL, from)->storage == STORAGE_TAGGED
				&& (tagged_select_subtype(NULL, from, to, true)
				|| tagged_subset_compat(NULL, from, to));
		is_valid_pointer = type_dealias(NULL, from)->storage == STORAGE_POINTER
				&& (type_dealias(NULL, to)->storage == STORAGE_POINTER
				|| type_dealias(NULL, to)->storage == STORAGE_NULL);
		assert(is_valid_tagged || is_valid_pointer);
		if (expr->cast.kind == C_TEST && is_valid_tagged) {
			return gen_type_assertion_or_test(ctx, expr,
					gen_expr(ctx, expr->cast.value));
		}
	}

	if (cast_prefers_at(expr)) {
		struct gen_value out = mkgtemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mkqval(ctx, &out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_expr_cast_at(ctx, expr, out);
		return out;
	}

	if (expr->cast.lowered) {
		pushc(ctx->current, "gen lowered cast");
	}

	// Special cases
	bool want_null = false;
	switch (type_dealias(NULL, to)->storage) {
	case STORAGE_NULL:
		want_null = true;
		// fallthrough
	case STORAGE_POINTER:
		if (type_dealias(NULL, from)->storage == STORAGE_SLICE) {
			struct gen_value value = gen_expr(ctx, expr->cast.value);
			value.type = to;
			return gen_load(ctx, value);
		}
		if (type_dealias(NULL, from)->storage != STORAGE_POINTER) {
			break;
		}

		struct gen_value val = gen_expr(ctx, expr->cast.value);
		struct qbe_value qval = mkqval(ctx, &val);
		struct qbe_value zero = constl(0);
		enum qbe_instr compare = want_null ? Q_CEQL : Q_CNEL;
		if (expr->cast.kind == C_TEST) {
			struct gen_value out =
				mkgtemp(ctx, &builtin_type_bool, ".%d");
			struct qbe_value qout = mkqval(ctx, &out);

			pushi(ctx->current, &qout, compare, &qval, &zero, NULL);
			return out;
		} else if (expr->cast.kind == C_ASSERTION) {
			struct qbe_statement failedl, passedl;
			struct qbe_value bfailed =
				mklabel(ctx, &failedl, "failed.%d");
			struct qbe_value bpassed =
				mklabel(ctx, &passedl, "passed.%d");

			struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");
			pushi(ctx->current, &cmpres, compare, &qval, &zero, NULL);
			pushi(ctx->current, NULL, Q_JNZ, &cmpres,
				&bpassed, &bfailed, NULL);
			push(&ctx->current->body, &failedl);
			gen_fixed_abort(ctx, expr->loc, ABORT_TYPE_ASSERTION);

			push(&ctx->current->body, &passedl);
			if (want_null) {
				return (struct gen_value){
					.kind = GV_CONST,
					.type = &builtin_type_null,
					.lval = 0,
				};
			}
		}
		val.type = to;
		return val;
	case STORAGE_VOID:
		gen_expr(ctx, expr->cast.value); // Side-effects
		return gv_void;
	default: break;
	}

	// Special case: tagged => non-tagged
	if (type_dealias(NULL, from)->storage == STORAGE_TAGGED) {
		struct gen_value value = gen_expr(ctx, expr->cast.value);
		struct qbe_value base = mkcopy(ctx, &value, ".%d");
		if (expr->cast.kind == C_ASSERTION) {
			gen_type_assertion_or_test(ctx, expr, value);
		}

		struct qbe_value align = nested_tagged_offset(
				expr->cast.value->result, expr->cast.secondary);
		pushi(ctx->current, &base, Q_ADD, &base, &align, NULL);
		struct gen_value storage = (struct gen_value){
			.kind = GV_TEMP,
			.type = to,
			.name = base.name,
		};
		return gen_load(ctx, storage);
	}

	// Special case: no conversion required
	if (type_dealias(NULL, to)->storage == type_dealias(NULL, from)->storage
			&& to->size == from->size) {
		struct gen_value value = gen_expr(ctx, expr->cast.value);
		value.type = to;
		return value;
	}

	struct gen_value value = gen_expr(ctx, expr->cast.value);
	struct qbe_value qvalue = mkqval(ctx, &value);
	struct gen_value result = mkgtemp(ctx, expr->result, "cast.%d");
	struct qbe_value qresult = mkqval(ctx, &result);
	struct gen_value intermediate;
	struct qbe_value qintermediate;

	from = lower_const(NULL, from, NULL);

	enum qbe_instr op;
	bool is_signed = type_is_signed(NULL, from);
	enum type_storage fstor = type_dealias(NULL, from)->storage,
		tstor = type_dealias(NULL, to)->storage;
	switch (tstor) {
	case STORAGE_ENUM:
	case STORAGE_U8:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_U16:
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_I64:
	case STORAGE_U64:
	case STORAGE_UINTPTR:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
		if (type_is_integer(NULL, from) && to->size <= from->size) {
			op = Q_COPY;
		} else if (type_is_integer(NULL, from) && to->size > from->size) {
			switch (from->size) {
			case 4: op = is_signed ? Q_EXTSW : Q_EXTUW; break;
			case 2: op = is_signed ? Q_EXTSH : Q_EXTUH; break;
			case 1: op = is_signed ? Q_EXTSB : Q_EXTUB; break;
			default: abort(); // Invariant
			}
		} else if (fstor == STORAGE_POINTER || fstor == STORAGE_NULL) {
			assert(tstor == STORAGE_UINTPTR);
			op = Q_COPY;
		} else if (fstor == STORAGE_RUNE) {
			op = Q_COPY;
		} else if (type_is_float(NULL, from)) {
			if (type_is_signed(NULL, to)) {
				switch (fstor) {
				case STORAGE_F32: op = Q_STOSI; break;
				case STORAGE_F64: op = Q_DTOSI; break;
				default: abort(); // Invariant
				}
			} else {
				switch (fstor) {
				case STORAGE_F32: op = Q_STOUI; break;
				case STORAGE_F64: op = Q_DTOUI; break;
				default: abort(); // Invariant
				}
			}
		} else {
			abort(); // Invariant
		}
		pushi(ctx->current, &qresult, op, &qvalue, NULL);
		break;
	case STORAGE_F32:
	case STORAGE_F64:
		if (type_is_float(NULL, from) && from->size == to->size) {
			op = Q_COPY;
		} else if (type_is_float(NULL, from) && to->size < from->size) {
			op = Q_TRUNCD;
		} else if (type_is_float(NULL, from) && to->size > from->size) {
			op = Q_EXTS;
		} else if (type_is_integer(NULL, from)) {
			if (type_is_signed(NULL, from)) {
				switch (from->size) {
				case 1:
				case 2:
					intermediate = mkgtemp(ctx,
						&builtin_type_i32, "cast.%d");
					qintermediate = mkqval(ctx, &intermediate);
					pushi(ctx->current, &qintermediate,
						from->size == 1? Q_EXTSB : Q_EXTSH,
						&qvalue, NULL);
					qvalue = qintermediate;
					/* fallthrough */
				case 4:
					op = Q_SWTOF;
					break;
				case 8:
					op = Q_SLTOF;
					break;
				default: abort(); // Invariant
				}
			} else {
				switch (from->size) {
				case 1:
				case 2:
					intermediate = mkgtemp(ctx,
						&builtin_type_i32, "cast.%d");
					qintermediate = mkqval(ctx, &intermediate);
					pushi(ctx->current, &qintermediate,
						from->size == 1? Q_EXTUB : Q_EXTUH,
						&qvalue, NULL);
					qvalue = qintermediate;
					/* fallthrough */
				case 4:
					op = Q_UWTOF;
					break;
				case 8:
					op = Q_ULTOF;
					break;
				default: abort(); // Invariant
				}
			}
		} else {
			abort(); // Invariant
		}
		pushi(ctx->current, &qresult, op, &qvalue, NULL);
		break;
	case STORAGE_NULL:
	case STORAGE_POINTER:
		pushi(ctx->current, &qresult, Q_COPY, &qvalue, NULL);
		break;
	case STORAGE_ARRAY:
		assert(from->storage == STORAGE_ARRAY);
		pushi(ctx->current, &qresult, Q_COPY, &qvalue, NULL);
		break;
	case STORAGE_SLICE:
		assert(from->storage == STORAGE_SLICE);
		pushi(ctx->current, &qresult, Q_COPY, &qvalue, NULL);
		break;
	case STORAGE_ALIAS:
	case STORAGE_BOOL:
	case STORAGE_ERROR:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_OPAQUE:
	case STORAGE_RCONST:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
	case STORAGE_VOID:
		abort(); // Invariant
	}

	return result;
}

static struct gen_value
gen_expr_compound_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct qbe_statement lend;
	struct qbe_value bend = mklabel(ctx, &lend, ".%d");
	struct gen_scope *scope = push_scope(ctx, expr->compound.scope);
	scope->end = &bend;

	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}
	scope->out = out;
	scope->result = gvout;

	const struct expressions *exprs;
	for (exprs = &expr->compound.exprs; exprs->next; exprs = exprs->next) {
		gen_expr(ctx, exprs->expr);
	}

	struct gen_value last = gen_expr_with(ctx, exprs->expr, out);
	branch_copyresult(ctx, last, gvout, out);
	pop_scope(ctx, !exprs->expr->terminates);
	push(&ctx->current->body, &lend);
	return gvout;
}

static void
gen_const_array_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	struct array_constant *aexpr = expr->constant.array;
	struct qbe_value base = mkqval(ctx, &out);

	size_t n = 0;
	const struct type *atype = type_dealias(NULL, expr->result);
	size_t msize = atype->array.members->size;
	struct gen_value item = mkgtemp(ctx, atype->array.members, "item.%d");
	for (const struct array_constant *ac = aexpr; ac; ac = ac->next) {
		struct qbe_value offs = constl(n * msize);
		struct qbe_value ptr = mklval(ctx, &item);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, ac->value, item);
		++n;
	}
	assert(n == atype->array.length);
	if (!atype->array.expandable || n == 0) {
		return;
	}
	assert(out.type);
	const struct type_array arr = type_dealias(NULL, out.type)->array;
	if (arr.length <= n) {
		return;
	}

	// last copied element
	struct qbe_value lsize = constl((n - 1) * msize);
	struct qbe_value last = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &last, Q_ADD, &base, &lsize, NULL);
	// start from the elements already copied
	struct qbe_value nsize = constl(n * msize);
	struct qbe_value next = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &next, Q_ADD, &base, &nsize, NULL);

	struct qbe_value qlen = constl((arr.length - n) * msize);
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memcpy, &next, &last, &qlen, NULL);
}

static void
gen_const_string_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	const struct expression_constant *constant = &expr->constant;
	const char *val = constant->string.value;
	size_t len = constant->string.len;

	// TODO: Generate string data structure as global also?
	struct qbe_value global = mkqtmp(ctx, ctx->arch.ptr, "strdata.%d");
	global.kind = QV_GLOBAL;

	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->name = global.name;
	def->kind = Q_DATA;
	def->data.align = ALIGN_UNDEFINED;
	def->data.items.type = QD_STRING;
	def->data.items.str = xcalloc(len, 1);
	memcpy(def->data.items.str, val, len);
	def->data.items.sz = len;

	if (len != 0) {
		qbe_append_def(ctx->out, def);
	} else {
		free(def);
		global = constl(0);
	}

	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	struct qbe_value strp = mkcopy(ctx, &out, ".%d");
	struct qbe_value qlen = constl(len);
	struct qbe_value offs = constl(builtin_type_size.size);
	pushi(ctx->current, NULL, store, &global, &strp, NULL);
	pushi(ctx->current, &strp, Q_ADD, &strp, &offs, NULL);
	pushi(ctx->current, NULL, store, &qlen, &strp, NULL);
	pushi(ctx->current, &strp, Q_ADD, &strp, &offs, NULL);
	pushi(ctx->current, NULL, store, &qlen, &strp, NULL);
}

static void
gen_const_struct_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	// TODO: Merge me into constant expressions
	struct qbe_value base = mkqval(ctx, &out);

	struct gen_value ftemp = mkgtemp(ctx, &builtin_type_void, "field.%d");
	for (const struct struct_constant *field = expr->constant._struct;
			field; field = field->next) {
		assert(field->value);

		struct qbe_value offs = constl(field->field->offset);
		ftemp.type = field->value->result;
		struct qbe_value ptr = mklval(ctx, &ftemp);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, field->value, ftemp);
	}
}

static void
gen_const_tagged_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	struct qbe_value qout = mklval(ctx, &out);
	const struct type *subtype = expr->constant.tagged.tag;
	struct qbe_value id = constw(subtype->id);
	enum qbe_instr store = store_for_type(ctx, &builtin_type_uint);
	pushi(ctx->current, NULL, store, &id, &qout, NULL);
	if (subtype->size == 0) {
		return;
	}

	struct gen_value storage = mkgtemp(ctx, subtype, ".%d");
	struct qbe_value qstor = mklval(ctx, &storage);
	struct qbe_value offs = constl(expr->result->align);
	pushi(ctx->current, &qstor, Q_ADD, &qout, &offs, NULL);
	gen_expr_at(ctx, expr->constant.tagged.value, storage);
}

static void
gen_const_tuple_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	// TODO: Merge me into constant expressions
	struct qbe_value base = mkqval(ctx, &out);

	struct gen_value ftemp = mkgtemp(ctx, &builtin_type_void, "field.%d");
	for (const struct tuple_constant *field = expr->constant.tuple; field;
			field = field->next) {
		assert(field->value);

		struct qbe_value offs = constl(field->field->offset);
		ftemp.type = field->value->result;
		struct qbe_value ptr = mklval(ctx, &ftemp);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, field->value, ftemp);
	}
}

static void
gen_expr_const_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	if (!type_is_aggregate(type_dealias(NULL, expr->result))) {
		gen_store(ctx, out, gen_expr(ctx, expr));
		return;
	}

	switch (type_dealias(NULL, expr->result)->storage) {
	case STORAGE_ARRAY:
		gen_const_array_at(ctx, expr, out);
		break;
	case STORAGE_STRING:
		gen_const_string_at(ctx, expr, out);
		break;
	case STORAGE_STRUCT:
		gen_const_struct_at(ctx, expr, out);
		break;
	case STORAGE_TAGGED:
		gen_const_tagged_at(ctx, expr, out);
		break;
	case STORAGE_TUPLE:
		gen_const_tuple_at(ctx, expr, out);
		break;
	default:
		abort(); // Invariant
	}
}

static struct gen_value
gen_expr_const(struct gen_context *ctx, const struct expression *expr)
{
	if (type_is_aggregate(type_dealias(NULL, expr->result))) {
		struct gen_value out = mkgtemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mkqval(ctx, &out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_expr_at(ctx, expr, out);
		return out;
	}

	struct gen_value val = {
		.kind = GV_CONST,
		.type = expr->result,
	};

	// Special cases
	switch (type_dealias(NULL, expr->result)->storage) {
	case STORAGE_BOOL:
		val.wval = expr->constant.bval ? 1 : 0;
		return val;
	case STORAGE_VOID:
		return val;
	case STORAGE_NULL:
		val.lval = 0;
		return val;
	default:
		// Moving right along
		break;
	}

	if (expr->constant.object != NULL) {
		assert(expr->constant.ival == 0);
		val = gen_access_ident(ctx, expr->constant.object);
		val.type = expr->result;
		return val;
	}

	const struct qbe_type *qtype = qtype_lookup(ctx, expr->result, false);
	switch (qtype->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
		val.wval = (uint32_t)expr->constant.uval;
		return val;
	case Q_LONG:
		val.lval = expr->constant.uval;
		return val;
	case Q_SINGLE:
		pushc(ctx->current, "%f", (float)expr->constant.fval);
		val.sval = (float)expr->constant.fval;
		return val;
	case Q_DOUBLE:
		pushc(ctx->current, "%f", expr->constant.fval);
		val.dval = expr->constant.fval;
		return val;
	case Q__VOID:
		return val;
	case Q__AGGREGATE:
		assert(0); // Invariant
	}

	abort(); // Invariant
}

static struct gen_value
gen_expr_defer(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_defer *defer = xcalloc(1, sizeof(struct gen_defer));
	defer->expr = expr;
	defer->next = ctx->scope->defers;
	ctx->scope->defers = defer;
	return gv_void;
}

static struct gen_value
gen_expr_delete(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value object, start;
	const struct expression *dexpr = expr->delete.expr;
	if (dexpr->type == EXPR_SLICE) {
		object = gen_expr(ctx, dexpr->slice.object);
		if (dexpr->slice.start) {
			start = gen_expr(ctx, dexpr->slice.start);
		} else {
			start.kind = GV_CONST;
			start.type = &builtin_type_size;
			start.lval = 0;
		}
	} else {
		assert(dexpr->type == EXPR_ACCESS
			&& dexpr->access.type == ACCESS_INDEX);
		object = gen_expr(ctx, dexpr->access.array);
		start = gen_expr(ctx, dexpr->access.index);
	}
	object = gen_autoderef(ctx, object);
	assert(type_dealias(NULL, object.type)->storage == STORAGE_SLICE);

	struct qbe_value qobj = mkqval(ctx, &object);
	struct qbe_value qlenptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value qlen = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value offset = constl(builtin_type_size.size);
	enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, &qlenptr, Q_ADD, &qobj, &offset, NULL);
	pushi(ctx->current, &qlen, load, &qlenptr, NULL);

	struct qbe_value qstart = mkqval(ctx, &start);
	struct qbe_value qend = {0};
	if (dexpr->type == EXPR_SLICE) {
		if (dexpr->slice.end) {
			struct gen_value end = gen_expr(ctx, dexpr->slice.end);
			qend = mkqval(ctx, &end);
		} else {
			qend = qlen;
		}
	} else {
		struct qbe_value tmp = constl(1);
		qend = mkqtmp(ctx, qstart.type, ".%d");
		pushi(ctx->current, &qend, Q_ADD, &qstart, &tmp, NULL);
	}

	struct qbe_value start_oob = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value end_oob = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value startend_oob = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value valid = mkqtmp(ctx, &qbe_word, ".%d");
	pushi(ctx->current, &start_oob, Q_CULEL, &qstart, &qlen, NULL);
	pushi(ctx->current, &end_oob, Q_CULEL, &qend, &qlen, NULL);
	pushi(ctx->current, &valid, Q_AND, &start_oob, &end_oob, NULL);
	pushi(ctx->current, &startend_oob, Q_CULEL, &qstart, &qend, NULL);
	pushi(ctx->current, &valid, Q_AND, &valid, &startend_oob, NULL);

	struct qbe_statement linvalid, lvalid;
	struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
	struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");

	pushi(ctx->current, NULL, Q_JNZ, &valid, &bvalid, &binvalid, NULL);
	push(&ctx->current->body, &linvalid);
	gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
	push(&ctx->current->body, &lvalid);

	struct qbe_value data = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value startptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value endptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value mlen = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &data, load, &qobj, NULL);
	struct qbe_value membsz =
		constl(type_dealias(NULL, object.type)->array.members->size);
	pushi(ctx->current, &startptr, Q_MUL, &qstart, &membsz, NULL);
	pushi(ctx->current, &startptr, Q_ADD, &startptr, &data, NULL);
	pushi(ctx->current, &endptr, Q_MUL, &qend, &membsz, NULL);
	pushi(ctx->current, &endptr, Q_ADD, &endptr, &data, NULL);
	pushi(ctx->current, &qlen, Q_SUB, &qlen, &qend, NULL);
	pushi(ctx->current, &mlen, Q_MUL, &qlen, &membsz, NULL);

	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove, &startptr, &endptr, &mlen,
		NULL);

	pushi(ctx->current, &qlen, Q_ADD, &qlen, &qstart, NULL);
	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, NULL, store, &qlen, &qlenptr, NULL);

	if (!expr->delete.is_static) {
		qobj = mklval(ctx, &object);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.unensure, &qobj, &membsz,
			NULL);
	}

	return gv_void;
}

static struct gen_value
gen_expr_for(struct gen_context *ctx, const struct expression *expr)
{
	struct qbe_statement lloop, lbody, lafter, lend;
	struct qbe_value bloop = mklabel(ctx, &lloop, "loop.%d");
	struct qbe_value bbody = mklabel(ctx, &lbody, "body.%d");
	struct qbe_value bend = mklabel(ctx, &lend, ".%d");
	struct qbe_value bafter = mklabel(ctx, &lafter, "after.%d");

	if (expr->_for.bindings) {
		gen_expr_binding(ctx, expr->_for.bindings);
	}

	push_scope(ctx, expr->_for.scope);
	ctx->scope->after = &bafter;
	ctx->scope->end = &bend;

	push(&ctx->current->body, &lloop);
	struct gen_value cond = gen_expr(ctx, expr->_for.cond);
	struct qbe_value qcond = mkqval(ctx, &cond);
	pushi(ctx->current, NULL, Q_JNZ, &qcond, &bbody, &bend, NULL);

	push(&ctx->current->body, &lbody);
	gen_expr(ctx, expr->_for.body);

	push(&ctx->current->body, &lafter);
	if (expr->_for.afterthought) {
		gen_expr(ctx, expr->_for.afterthought);
	}

	pop_scope(ctx, true);

	pushi(ctx->current, NULL, Q_JMP, &bloop, NULL);

	push(&ctx->current->body, &lend);
	return gv_void;
}

static struct gen_value
gen_expr_free(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *type = type_dealias(NULL, expr->free.expr->result);
	struct gen_value val = gen_expr(ctx, expr->free.expr);
	struct qbe_value qval = mkqval(ctx, &val);
	if (type->storage == STORAGE_SLICE || type->storage == STORAGE_STRING) {
		struct qbe_value lval = mklval(ctx, &val);
		qval = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &qval, Q_LOADL, &lval, NULL);
	}
	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.free, &qval, NULL);
	return gv_void;
}

static struct gen_value
gen_expr_if_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}

	struct qbe_statement ltrue, lfalse, lend;
	struct qbe_value btrue = mklabel(ctx, &ltrue, "true.%d");
	struct qbe_value bfalse = mklabel(ctx, &lfalse, "false.%d");
	struct qbe_value bend = mklabel(ctx, &lend, ".%d");
	struct gen_value cond = gen_expr(ctx, expr->_if.cond);
	struct qbe_value qcond = mkqval(ctx, &cond);
	qcond = extend(ctx, qcond, &builtin_type_bool);
	pushi(ctx->current, NULL, Q_JNZ, &qcond, &btrue, &bfalse, NULL);

	push(&ctx->current->body, &ltrue);
	struct gen_value vtrue = gen_expr_with(ctx, expr->_if.true_branch, out);
	branch_copyresult(ctx, vtrue, gvout, out);
	if (!expr->_if.true_branch->terminates) {
		pushi(ctx->current, NULL, Q_JMP, &bend, NULL);
	}

	push(&ctx->current->body, &lfalse);
	if (expr->_if.false_branch) {
		struct gen_value vfalse = gen_expr_with(ctx,
				expr->_if.false_branch, out);
		branch_copyresult(ctx, vfalse, gvout, out);
	}

	push(&ctx->current->body, &lend);
	return gvout;
}

static struct gen_value
gen_expr_insert(struct gen_context *ctx, const struct expression *expr)
{
	// XXX: A lot of this code is identical to the corresponding append
	// code, maybe we can/should deduplicate it
	assert(expr->append.length == NULL);
	const struct expression *objexpr = expr->append.object;
	assert(objexpr->type == EXPR_ACCESS
			&& objexpr->access.type == ACCESS_INDEX);
	const struct expression *arexpr = objexpr->access.array;
	struct gen_value slice = gen_expr(ctx, arexpr);
	slice = gen_autoderef(ctx, slice);
	struct qbe_value qslice = mkqval(ctx, &slice);
	struct gen_value index = gen_expr(ctx, objexpr->access.index);
	struct qbe_value qindex = mkqval(ctx, &index);

	struct qbe_value lenptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value prevlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
	struct qbe_value offs = constl(builtin_type_size.size);
	pushi(ctx->current, &lenptr, Q_ADD, &qslice, &offs, NULL);
	enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, &prevlen, load, &lenptr, NULL);

	struct qbe_value appendlen = constl(1);
	const struct type *valtype = type_dealias(NULL, expr->append.value->result);

	struct gen_value value;
	struct qbe_value qvalue;
	if (!expr->append.is_multi || valtype->storage != STORAGE_ARRAY) {
		// We use gen_expr_at for the array case to avoid a copy
		value = gen_expr(ctx, expr->append.value);
		qvalue = mkqval(ctx, &value);
	}

	if (expr->append.is_multi) {
		if (valtype->storage == STORAGE_ARRAY) {
			assert(valtype->array.length != SIZE_UNDEFINED);
			appendlen = constl(valtype->array.length);
		} else {
			appendlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
			struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
			offs = constl(builtin_type_size.size);
			pushi(ctx->current, &ptr, Q_ADD, &qvalue, &offs, NULL);
			pushi(ctx->current, &appendlen, load, &ptr, NULL);
		}
	}

	struct qbe_value newlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &newlen, Q_ADD, &prevlen, &appendlen, NULL);
	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, NULL, store, &newlen, &lenptr, NULL);

	struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	const struct type *mtype = type_dealias(NULL, slice.type)->array.members;
	struct qbe_value membsz = constl(mtype->size);
	if (!expr->append.is_static) {
		struct qbe_value lval = mklval(ctx, &slice);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.ensure, &lval, &membsz, NULL);
	} else {
		offs = constl(builtin_type_size.size * 2);
		pushi(ctx->current, &ptr, Q_ADD, &qslice, &offs, NULL);
		struct qbe_value cap = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &cap, load, &ptr, NULL);

		struct qbe_statement lvalid, linvalid;
		struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");
		struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
		struct qbe_value valid = mkqtmp(ctx, &qbe_word, ".%d");
		pushi(ctx->current, &valid, Q_CULEL, &newlen, &cap, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &valid, &bvalid, &binvalid, NULL);

		push(&ctx->current->body, &linvalid);
		gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
		push(&ctx->current->body, &lvalid);
	}

	struct qbe_value base = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &base, load, &qslice, NULL);

	pushi(ctx->current, &ptr, Q_MUL, &qindex, &membsz, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &base, &ptr, NULL);

	struct qbe_value nbyte = mkqtmp(ctx, ctx->arch.sz, ".%d");
	struct qbe_value dest = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value ncopy = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &nbyte, Q_MUL, &appendlen, &membsz, NULL);
	pushi(ctx->current, &ncopy, Q_SUB, &prevlen, &qindex, NULL);
	pushi(ctx->current, &ncopy, Q_MUL, &ncopy, &membsz, NULL);
	pushi(ctx->current, &dest, Q_ADD, &ptr, &nbyte, NULL);

	pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove, &dest, &ptr, &ncopy, NULL);

	struct gen_value item = {
		.kind = GV_TEMP,
		.type = mtype,
		.name = ptr.name,
	};
	if (expr->append.is_multi && valtype->storage == STORAGE_ARRAY) {
		item.type = valtype;
		gen_expr_at(ctx, expr->append.value, item);
	} else if (expr->append.is_multi && valtype->storage == STORAGE_SLICE) {
		struct qbe_value qsrc = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &qsrc, load, &qvalue, NULL);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memmove, &ptr, &qsrc, &nbyte, NULL);
	} else {
		gen_store(ctx, item, value);
	}

	return gv_void;
}

enum match_compat {
	// The case type is a member of the match object type and can be used
	// directly from the match object's tagged union storage area.
	COMPAT_SUBTYPE,
	// The case type is a tagged union which is a subset of the object type,
	// and alignment-compatible. In this case we can use the match object
	// directly.
	COMPAT_ALIGNED,
	// The case type is a tagged union which is a subset of the object type,
	// but with a different alignment. In this case we must convert the
	// match object before using it.
	COMPAT_MISALIGNED,
};

static struct qbe_value
nested_tagged_offset(const struct type *tu, const struct type *target)
{
	// This function calculates the offset of a member in a nested tagged union
	//
	// type foo = (int | void);
	// type bar = (size | foo);
	//
	// Offset of the foo field from the start of bar is 8 and offset of int
	// inside foo is 4, so the offset of the int from the start of bar is 12
	const struct type *test = tu;
	struct qbe_value offset = constl(tu->align);
	do {
		test = tagged_select_subtype(NULL, tu, target, false);
		if (!test) {
			break;
		}
		if (test->id != target->id && type_dealias(NULL, test)->id != target->id) {
			offset.lval += test->align;
		}
		tu = test;
	} while (test->id != target->id && type_dealias(NULL, test)->id != target->id);
	return offset;
}

static struct gen_value
gen_nested_match_tests(struct gen_context *ctx, struct gen_value object,
	struct qbe_value bmatch, struct qbe_value bnext,
	struct qbe_value tag, const struct type *type)
{
	// This function handles the case where we're matching against a type
	// which is a member of the tagged union, or an inner tagged union.
	//
	// type foo = (int | void);
	// type bar = (size | foo);
	//
	// let x: bar = 10i;
	// match (x) {
	// case let z: size => ...
	// case let i: int => ...
	// case	void => ...
	// };
	//
	// In the first case, we can simply test the object's tag. In the second
	// case, we have to test if the selected tag is 'foo', then check the
	// tag of the foo object for int.
	struct qbe_value *subtag = &tag;
	struct qbe_value subval = mkcopy(ctx, &object, "subval.%d");
	struct gen_value match = mkgtemp(ctx, &builtin_type_bool, ".%d");
	struct qbe_value qmatch = mkqval(ctx, &match);
	struct qbe_value temp = mkqtmp(ctx, &qbe_word, ".%d");
	const struct type *subtype = object.type;
	const struct type *test = type;
	do {
		struct qbe_statement lsubtype;
		struct qbe_value bsubtype = mklabel(ctx, &lsubtype, "subtype.%d");

		if (type_dealias(NULL, subtype)->storage != STORAGE_TAGGED) {
			break;
		}
		test = tagged_select_subtype(NULL, subtype, type, false);
		if (!test) {
			break;
		}

		struct qbe_value id = constw(test->id);
		pushi(ctx->current, &qmatch, Q_CEQW, subtag, &id, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &qmatch, &bsubtype, &bnext, NULL);
		push(&ctx->current->body, &lsubtype);

		if (test->id != type->id && type_dealias(NULL, test)->id != type->id) {
			struct qbe_value offs = constl(subtype->align);
			pushi(ctx->current, &subval, Q_ADD, &subval, &offs, NULL);
			pushi(ctx->current, &temp, Q_LOADUW, &subval, NULL);
			subtag = &temp;
		}

		subtype = test;
	} while (test->id != type->id && type_dealias(NULL, test)->id != type->id);

	pushi(ctx->current, NULL, Q_JMP, &bmatch, NULL);
	return match;
}

static struct gen_value
gen_subset_match_tests(struct gen_context *ctx,
	struct qbe_value bmatch, struct qbe_value bnext,
	struct qbe_value tag, const struct type *type)
{
	// In this case, we're testing a case which is itself a tagged union,
	// and is a subset of the match object.
	//
	// type foo = (size | int | void);
	//
	// let x: foo = 10i;
	// match (x) {
	// case let n: (size | int) => ...
	// case void => ...
	// };
	//
	// In this situation, we test the match object's tag against each type
	// ID of the case type.
	struct gen_value match = mkgtemp(ctx, &builtin_type_bool, ".%d");
	for (const struct type_tagged_union *tu = &type->tagged; tu; tu = tu->next) {
		struct qbe_statement lnexttag;
		struct qbe_value bnexttag = mklabel(ctx, &lnexttag, ".%d");
		struct qbe_value id = constl(tu->type->id);
		struct qbe_value qmatch = mkqval(ctx, &match);
		pushi(ctx->current, &qmatch, Q_CEQW, &tag, &id, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &qmatch, &bmatch, &bnexttag, NULL);
		push(&ctx->current->body, &lnexttag);
	}
	pushi(ctx->current, NULL, Q_JMP, &bnext, NULL);
	return match;
}

static struct gen_value
gen_match_with_tagged(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}

	const struct type *objtype = expr->match.value->result;
	struct gen_value object = gen_expr(ctx, expr->match.value);
	struct qbe_value qobject = mkqval(ctx, &object);
	struct qbe_value tag = mkqtmp(ctx, ctx->arch.sz, "tag.%d");
	enum qbe_instr load = load_for_type(ctx, &builtin_type_uint);
	pushi(ctx->current, &tag, load, &qobject, NULL);

	struct qbe_statement lout;
	struct qbe_value bout = mklabel(ctx, &lout, ".%d");

	struct gen_value bval;
	const struct match_case *_default = NULL;
	for (const struct match_case *_case = expr->match.cases;
			_case; _case = _case->next) {
		if (!_case->type) {
			_default = _case;
			continue;
		}

		struct qbe_statement lmatch, lnext;
		struct qbe_value bmatch = mklabel(ctx, &lmatch, "matches.%d");
		struct qbe_value bnext = mklabel(ctx, &lnext, "next.%d");
		const struct type *subtype =
			tagged_select_subtype(NULL, objtype, _case->type, false);
		enum match_compat compat = COMPAT_SUBTYPE;
		if (subtype) {
			gen_nested_match_tests(ctx, object,
				bmatch, bnext, tag, _case->type);
		} else {
			assert(type_dealias(NULL, _case->type)->storage == STORAGE_TAGGED);
			assert(tagged_subset_compat(NULL, objtype, _case->type));
			if (tagged_align_compat(objtype, _case->type)) {
				compat = COMPAT_ALIGNED;
			} else {
				compat = COMPAT_MISALIGNED;
			}
			const struct type *casetype = type_dealias(NULL, _case->type);
			gen_subset_match_tests(ctx, bmatch, bnext, tag, casetype);
		}

		push(&ctx->current->body, &lmatch);

		if (!_case->object) {
			goto next;
		}

		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value.kind = GV_TEMP;
		gb->value.type = _case->type;
		gb->value.name = gen_name(&ctx->id, "binding.%d");
		gb->object = _case->object;
		gb->next = ctx->bindings;
		ctx->bindings = gb;

		struct qbe_value qv = mklval(ctx, &gb->value);
		enum qbe_instr alloc = alloc_for_align(_case->type->align);
		struct qbe_value sz = constl(_case->type->size);
		pushprei(ctx->current, &qv, alloc, &sz, NULL);

		struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		struct gen_value src = {
			.kind = GV_TEMP,
			.type = _case->type,
			.name = ptr.name,
		};
		struct gen_value load;
		struct qbe_value offset;
		switch (compat) {
		case COMPAT_SUBTYPE:
			offset = nested_tagged_offset(object.type, _case->type);
			pushi(ctx->current, &ptr, Q_ADD, &qobject, &offset, NULL);
			load = gen_load(ctx, src);
			gen_store(ctx, gb->value, load);
			break;
		case COMPAT_ALIGNED:
			pushi(ctx->current, &ptr, Q_COPY, &qobject, NULL);
			load = gen_load(ctx, src);
			gen_store(ctx, gb->value, load);
			break;
		case COMPAT_MISALIGNED:
			;
			struct expression value = {
				.type = EXPR_GEN_VALUE,
				.result = objtype,
				.user = &object,
			};
			struct expression cast = {
				.type = EXPR_CAST,
				.result = _case->type,
				.cast = {
					.kind = C_CAST,
					.secondary = _case->type,
					.value = &value,
				},
			};
			gen_expr_at(ctx, &cast, gb->value);
			break;
		}

next:
		bval = gen_expr_with(ctx, _case->value, out);
		branch_copyresult(ctx, bval, gvout, out);
		if (!_case->value->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
		push(&ctx->current->body, &lnext);
	}

	if (_default) {
		bval = gen_expr_with(ctx, _default->value, out);
		branch_copyresult(ctx, bval, gvout, out);
	} else {
		struct qbe_statement labort;
		mklabel(ctx, &labort, ".%d");
		push(&ctx->current->body, &labort);
		gen_fixed_abort(ctx, expr->loc, ABORT_UNREACHABLE);
	}

	push(&ctx->current->body, &lout);
	return gvout;
}

static struct gen_value
gen_match_with_nullable(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}

	struct qbe_statement lout;
	struct qbe_value bout = mklabel(ctx, &lout, ".%d");
	struct gen_value object = gen_expr(ctx, expr->match.value);
	struct qbe_value qobject = mkqval(ctx, &object);
	struct qbe_value zero = constl(0);

	struct gen_value bval;
	const struct match_case *_default = NULL;
	for (const struct match_case *_case = expr->match.cases;
			_case; _case = _case->next) {
		if (!_case->type) {
			_default = _case;
			continue;
		}

		struct qbe_statement lmatch, lnext;
		struct qbe_value cmpres = mkqtmp(ctx, &qbe_word, ".%d");
		struct qbe_value bmatch = mklabel(ctx, &lmatch, "matches.%d");
		struct qbe_value bnext = mklabel(ctx, &lnext, "next.%d");

		enum qbe_instr compare;
		if (_case->type->storage == STORAGE_NULL) {
			compare = Q_CEQL;
		} else {
			compare = Q_CNEL;
		}
		pushi(ctx->current, &cmpres, compare, &qobject, &zero, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &cmpres, &bmatch, &bnext, NULL);

		push(&ctx->current->body, &lmatch);

		if (!_case->object) {
			goto next;
		}

		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value = mkgtemp(ctx, _case->type, "binding.%d");
		gb->object = _case->object;
		gb->next = ctx->bindings;
		ctx->bindings = gb;

		enum qbe_instr store = store_for_type(ctx, _case->type);
		enum qbe_instr alloc = alloc_for_align(_case->type->align);
		struct qbe_value qv = mkqval(ctx, &gb->value);
		struct qbe_value sz = constl(_case->type->size);
		pushprei(ctx->current, &qv, alloc, &sz, NULL);
		pushi(ctx->current, NULL, store, &qobject, &qv, NULL);

next:
		bval = gen_expr_with(ctx, _case->value, out);
		branch_copyresult(ctx, bval, gvout, out);
		if (!_case->value->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
		push(&ctx->current->body, &lnext);
	}

	if (_default) {
		bval = gen_expr_with(ctx, _default->value, out);
		branch_copyresult(ctx, bval, gvout, out);
		if (!_default->value->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
	}

	struct qbe_statement labort;
	mklabel(ctx, &labort, ".%d");
	push(&ctx->current->body, &labort);
	gen_fixed_abort(ctx, expr->loc, ABORT_UNREACHABLE);

	push(&ctx->current->body, &lout);
	return gvout;
}

static struct gen_value
gen_expr_match_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	const struct type *objtype = expr->match.value->result;
	switch (type_dealias(NULL, objtype)->storage) {
	case STORAGE_POINTER:
		return gen_match_with_nullable(ctx, expr, out);
	case STORAGE_TAGGED:
		return gen_match_with_tagged(ctx, expr, out);
	default: abort(); // Invariant
	}
}

static struct gen_value
gen_expr_measure(struct gen_context *ctx, const struct expression *expr)
{
	size_t len;
	struct gen_value gv, temp;
	const struct type *type;
	const struct expression *value = expr->measure.value;
	switch (expr->measure.op) {
	case M_LEN:
		type = type_dealias(NULL, type_dereference(NULL, value->result));
		switch (type->storage) {
		case STORAGE_ARRAY:
			len = type->array.length;
			assert(len != SIZE_UNDEFINED);
			return (struct gen_value){
				.kind = GV_CONST,
				.type = &builtin_type_size,
				.lval = len,
			};
		case STORAGE_SLICE:
		case STORAGE_STRING:
			gv = gen_expr(ctx, value);
			gv = gen_autoderef(ctx, gv);
			temp = mkgtemp(ctx, &builtin_type_size, ".%d");
			struct qbe_value qv = mkqval(ctx, &gv),
				qtemp = mkqval(ctx, &temp),
				offs = constl(builtin_type_size.size);
			enum qbe_instr load = load_for_type(ctx,
				&builtin_type_size);
			pushi(ctx->current, &qtemp, Q_ADD, &qv, &offs, NULL);
			pushi(ctx->current, &qtemp, load, &qtemp, NULL);
			return temp;
		default:
			abort(); // Invariant
		}
		break;
	case M_ALIGN:
		return (struct gen_value){
			.kind = GV_CONST,
			.type = &builtin_type_size,
			.lval = expr->measure.dimensions.align,
		};
	case M_SIZE:
		return (struct gen_value){
			.kind = GV_CONST,
			.type = &builtin_type_size,
			.lval = expr->measure.dimensions.size,
		};
	case M_OFFSET:
		if (expr->measure.value->access.type == ACCESS_FIELD) {
			return (struct gen_value){
				.kind = GV_CONST,
				.type = &builtin_type_size,
				.lval = expr->measure.value->access.field->offset,
			};
		} else {
			assert(expr->measure.value->access.type == ACCESS_TUPLE);
			return (struct gen_value){
				.kind = GV_CONST,
				.type = &builtin_type_size,
				.lval = expr->measure.value->access.tvalue->offset,
			};
		}
	}
	abort(); // Invariant
}

static struct gen_value
gen_expr_return(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value ret = gen_expr(ctx, expr->_return.value);
	for (struct gen_scope *scope = ctx->scope; scope; scope = scope->parent) {
		gen_defers(ctx, scope);
	}
	if (type_dealias(NULL, ret.type)->storage == STORAGE_VOID) {
		pushi(ctx->current, NULL, Q_RET, NULL);
	} else {
		struct qbe_value qret = mkqval(ctx, &ret);
		pushi(ctx->current, NULL, Q_RET, &qret, NULL);
	}
	return gv_void;
}

static void
gen_expr_struct_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	// TODO: Merge me into constant expressions
	struct qbe_value base = mkqval(ctx, &out);

	if (expr->_struct.autofill) {
		struct qbe_value size =
			constl(expr->result->size), zero = constl(0);
		struct qbe_value base = mklval(ctx, &out);
		pushi(ctx->current, NULL, Q_CALL, &ctx->rt.memset,
			&base, &zero, &size, NULL);
	}

	struct gen_value ftemp = mkgtemp(ctx, &builtin_type_void, "field.%d");
	for (const struct expr_struct_field *field = expr->_struct.fields;
			field; field = field->next) {
		if (!field->value) {
			assert(expr->_struct.autofill);
			continue;
		}

		struct qbe_value offs = constl(field->field->offset);
		ftemp.type = field->value->result;
		struct qbe_value ptr = mklval(ctx, &ftemp);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, field->value, ftemp);
	}
}

static struct gen_value
gen_expr_switch_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mkgtemp(ctx, expr->result, ".%d");
	}

	struct qbe_statement lout;
	struct qbe_value bout = mklabel(ctx, &lout, ".%d");
	struct gen_value value = gen_expr(ctx, expr->_switch.value);

	struct gen_value bval;
	const struct switch_case *_default = NULL;
	for (const struct switch_case *_case = expr->_switch.cases;
			_case; _case = _case->next) {
		if (!_case->options) {
			_default = _case;
			continue;
		}

		struct qbe_statement lmatch, lnextcase;
		struct qbe_value bmatch = mklabel(ctx, &lmatch, "matches.%d");
		struct qbe_value bnextcase = mklabel(ctx, &lnextcase, "next.%d");

		for (struct case_option *opt = _case->options;
				opt; opt = opt->next) {
			struct qbe_statement lnextopt;
			struct qbe_value bnextopt = mklabel(ctx, &lnextopt, ".%d");
			struct gen_value test = gen_expr_const(ctx, opt->value);
			struct expression lvalue = {
				.type = EXPR_GEN_VALUE,
				.result = value.type,
				.user = &value,
			}, rvalue = {
				.type = EXPR_GEN_VALUE,
				.result = test.type,
				.user = &test,
			}, compare = {
				.type = EXPR_BINARITHM,
				.result = &builtin_type_bool,
				.binarithm = {
					.op = BIN_LEQUAL,
					.lvalue = &lvalue,
					.rvalue = &rvalue,
				},
			};
			struct gen_value match = gen_expr(ctx, &compare);
			struct qbe_value cond = mkqval(ctx, &match);
			pushi(ctx->current, NULL, Q_JNZ,
				&cond, &bmatch, &bnextopt, NULL);
			push(&ctx->current->body, &lnextopt);
		}

		pushi(ctx->current, NULL, Q_JMP, &bnextcase, NULL);
		push(&ctx->current->body, &lmatch);
		bval = gen_expr_with(ctx, _case->value, out);
		branch_copyresult(ctx, bval, gvout, out);
		if (!_case->value->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
		push(&ctx->current->body, &lnextcase);
	}

	if (_default) {
		bval = gen_expr_with(ctx, _default->value, out);
		branch_copyresult(ctx, bval, gvout, out);
		if (!_default->value->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &bout, NULL);
		}
	}

	struct qbe_statement labort;
	mklabel(ctx, &labort, ".%d");
	push(&ctx->current->body, &labort);
	gen_fixed_abort(ctx, expr->loc, ABORT_UNREACHABLE);

	push(&ctx->current->body, &lout);
	return gvout;
}

static void
gen_expr_slice_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	struct gen_value object = gen_expr(ctx, expr->slice.object);
	object = gen_autoderef(ctx, object);
	const struct type *srctype = type_dealias(NULL, object.type);

	bool hasstart = expr->slice.start, hasend = expr->slice.end;
	bool check_bounds = !expr->slice.bounds_checked && (hasstart || hasend);
	struct gen_value length;
	struct qbe_value qlength;
	struct qbe_value qbase;
	struct qbe_value qobject = mkqval(ctx, &object);
	struct qbe_value offset = constl(builtin_type_size.size);
	struct qbe_value qptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	switch (srctype->storage) {
	case STORAGE_ARRAY:
		if (srctype->array.length != SIZE_UNDEFINED) {
			length = (struct gen_value){
				.kind = GV_CONST,
				.type = &builtin_type_size,
				.lval = srctype->array.length,
			};
			qlength = mkqval(ctx, &length);
		} else {
			assert(expr->slice.end);
			check_bounds = false;
		}
		qbase = mkqval(ctx, &object);
		break;
	case STORAGE_SLICE:
		qbase = mkqtmp(ctx, ctx->arch.sz, "base.%d");
		enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
		pushi(ctx->current, &qbase, load, &qobject, NULL);
		length = mkgtemp(ctx, &builtin_type_size, "len.%d");
		qlength = mkqval(ctx, &length);
		pushi(ctx->current, &qptr, Q_ADD, &qobject, &offset, NULL);
		pushi(ctx->current, &qlength, load, &qptr, NULL);
		break;
	default: abort(); // Invariant
	}

	struct gen_value start;
	if (hasstart) {
		start = gen_expr(ctx, expr->slice.start);
	} else {
		start = (struct gen_value){
			.kind = GV_CONST,
			.type = &builtin_type_size,
			.lval = 0,
		};
	}

	struct gen_value end;
	if (hasend) {
		end = gen_expr(ctx, expr->slice.end);
	} else {
		end = length;
	}

	struct qbe_value qstart = mkqval(ctx, &start);
	struct qbe_value qend = mkqval(ctx, &end);

	if (check_bounds) {
		struct qbe_value end_oob = mkqtmp(ctx, &qbe_word, ".%d");
		struct qbe_value start_oob = mkqtmp(ctx, &qbe_word, ".%d");
		struct qbe_value valid = mkqtmp(ctx, &qbe_word, ".%d");
		if (hasstart && hasend) {
			pushi(ctx->current, &start_oob, Q_CULEL, &qstart, &qend, NULL);
			pushi(ctx->current, &end_oob, Q_CULEL, &qend, &qlength, NULL);
			pushi(ctx->current, &valid, Q_AND, &start_oob, &end_oob, NULL);
		} else if (hasstart) {
			pushi(ctx->current, &valid, Q_CULEL, &qstart, &qlength, NULL);
		} else if (hasend) {
			pushi(ctx->current, &valid, Q_CULEL, &qend, &qlength, NULL);
		}

		struct qbe_statement linvalid, lvalid;
		struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
		struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");

		pushi(ctx->current, NULL, Q_JNZ, &valid, &bvalid, &binvalid, NULL);
		push(&ctx->current->body, &linvalid);
		gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
		push(&ctx->current->body, &lvalid);
	}

	struct qbe_value isz = constl(srctype->array.members->size);

	struct qbe_value qout = mkqval(ctx, &out);
	struct qbe_value data = mkqtmp(ctx, ctx->arch.ptr, "data.%d");
	pushi(ctx->current, &data, Q_MUL, &qstart, &isz, NULL);
	pushi(ctx->current, &data, Q_ADD, &qbase, &data, NULL);

	struct qbe_value newlen = mkqtmp(ctx, ctx->arch.sz, "newlen.%d");
	pushi(ctx->current, &newlen, Q_SUB, &qend, &qstart, NULL);
	struct qbe_value newcap = mkqtmp(ctx, ctx->arch.sz, "newcap.%d");
	if (check_bounds) {
		pushi(ctx->current, &newcap, Q_SUB, &qlength, &qstart, NULL);
	} else {
		pushi(ctx->current, &newcap, Q_COPY, &newlen, NULL);
	}

	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, NULL, store, &data, &qout, NULL);
	pushi(ctx->current, &qptr, Q_ADD, &qout, &offset, NULL);
	pushi(ctx->current, NULL, store, &newlen, &qptr, NULL);
	offset = constl(builtin_type_size.size * 2);
	pushi(ctx->current, &qptr, Q_ADD, &qout, &offset, NULL);
	pushi(ctx->current, NULL, store, &newcap, &qptr, NULL);
}

static void
gen_expr_tuple_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	// TODO: Merge me into constant expressions
	struct qbe_value base = mkqval(ctx, &out);

	const struct type *type = type_dealias(NULL, expr->result);
	struct gen_value vtemp = mkgtemp(ctx, &builtin_type_void, "value.%d");
	const struct expression_tuple *value = &expr->tuple;
	for (const struct type_tuple *tuple = &type->tuple;
			tuple; tuple = tuple->next) {
		struct qbe_value offs = constl(tuple->offset);
		vtemp.type = value->value->result;
		struct qbe_value ptr = mklval(ctx, &vtemp);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, value->value, vtemp);
		value = value->next;
	}
}

static struct gen_value
gen_expr_unarithm(struct gen_context *ctx,
	const struct expression *expr)
{
	struct gen_value val, temp;
	struct qbe_value qval, qtmp;
	const struct expression *operand = expr->unarithm.operand;
	switch (expr->unarithm.op) {
	case UN_ADDRESS:
		if (operand->type == EXPR_ACCESS) {
			val = gen_expr_access_addr(ctx, operand);
			val.type = expr->result;
			return val;
		}
		struct gen_value val = mkgtemp(ctx, operand->result, ".%d");
		struct qbe_value qv = mklval(ctx, &val);
		struct qbe_value sz = constl(val.type->size);
		enum qbe_instr alloc = alloc_for_align(val.type->align);
		pushprei(ctx->current, &qv, alloc, &sz, NULL);
		gen_expr_at(ctx, operand, val);
		val.type = expr->result;
		return val;
	case UN_DEREF:
		val = gen_expr(ctx, operand);
		assert(type_dealias(NULL, val.type)->storage == STORAGE_POINTER);
		val.type = type_dealias(NULL, val.type)->pointer.referent;
		return gen_load(ctx, val);
	case UN_BNOT:
		val = gen_expr(ctx, operand);
		temp = mkgtemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		struct qbe_value ones = constl((uint64_t)-1);
		pushi(ctx->current, &qtmp, Q_XOR, &qval, &ones, NULL);
		return temp;
	case UN_LNOT:
		val = gen_expr(ctx, operand);
		temp = mkgtemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		qval = extend(ctx, qval, operand->result);
		struct qbe_value zerow = constw(0);
		pushi(ctx->current, &qtmp, Q_CEQW, &qval, &zerow, NULL);
		return temp;
	case UN_MINUS:
		val = gen_expr(ctx, operand);
		temp = mkgtemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		pushi(ctx->current, &qtmp, Q_NEG, &qval, NULL);
		return temp;
	}
	abort(); // Invariant
}

static struct gen_value
gen_expr_vaarg(struct gen_context *ctx,
	const struct expression *expr)
{
	// XXX: qbe only supports variadic base types, should check for this
	struct gen_value result = mkgtemp(ctx, expr->result, ".%d");
	struct qbe_value qresult = mkqval(ctx, &result);
	struct gen_value ap = gen_expr(ctx, expr->vaarg.ap);
	struct qbe_value qap = mkqval(ctx, &ap);
	pushi(ctx->current, &qresult, Q_VAARG, &qap, NULL);
	return result;
}

static void
gen_expr_vastart_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	struct qbe_value base = mklval(ctx, &out);
	pushi(ctx->current, NULL, Q_VASTART, &base, NULL);
}

static struct gen_value
gen_expr(struct gen_context *ctx, const struct expression *expr)
{
	if (expr->loc.file && expr->loc.lineno) {
		struct qbe_value qline = constl(expr->loc.lineno);
		pushi(ctx->current, NULL, Q_DBGLOC, &qline, NULL);
	}

	switch ((int)expr->type) {
	case EXPR_ACCESS:
		return gen_expr_access(ctx, expr);
	case EXPR_ALLOC:
		return gen_expr_alloc_with(ctx, expr, NULL);
	case EXPR_APPEND:
		return gen_expr_append(ctx, expr);
	case EXPR_ASSERT:
		return gen_expr_assert(ctx, expr);
	case EXPR_ASSIGN:
		return gen_expr_assign(ctx, expr);
	case EXPR_BINARITHM:
		return gen_expr_binarithm(ctx, expr);
	case EXPR_BINDING:
		return gen_expr_binding(ctx, expr);
	case EXPR_BREAK:
	case EXPR_CONTINUE:
	case EXPR_YIELD:
		return gen_expr_control(ctx, expr);
	case EXPR_CALL:
		return gen_expr_call(ctx, expr);
	case EXPR_CAST:
		return gen_expr_cast(ctx, expr);
	case EXPR_COMPOUND:
		return gen_expr_compound_with(ctx, expr, NULL);
	case EXPR_CONSTANT:
		return gen_expr_const(ctx, expr);
	case EXPR_DEFER:
		return gen_expr_defer(ctx, expr);
	case EXPR_DELETE:
		return gen_expr_delete(ctx, expr);
	case EXPR_FOR:
		return gen_expr_for(ctx, expr);
	case EXPR_FREE:
		return gen_expr_free(ctx, expr);
	case EXPR_IF:
		return gen_expr_if_with(ctx, expr, NULL);
	case EXPR_INSERT:
		return gen_expr_insert(ctx, expr);
	case EXPR_MATCH:
		return gen_expr_match_with(ctx, expr, NULL);
	case EXPR_MEASURE:
		return gen_expr_measure(ctx, expr);
	case EXPR_PROPAGATE:
		assert(0); // Lowered in check (for now?)
	case EXPR_RETURN:
		return gen_expr_return(ctx, expr);
	case EXPR_SWITCH:
		return gen_expr_switch_with(ctx, expr, NULL);
	case EXPR_UNARITHM:
		return gen_expr_unarithm(ctx, expr);
	case EXPR_VAARG:
		return gen_expr_vaarg(ctx, expr);
	case EXPR_VAEND:
		return gv_void; // no-op
	case EXPR_SLICE:
	case EXPR_STRUCT:
	case EXPR_TUPLE:
	case EXPR_VASTART:
		break; // Prefers -at style
	// gen-specific psuedo-expressions
	case EXPR_GEN_VALUE:
		return *(struct gen_value *)expr->user;
	}

	struct gen_value out = mkgtemp(ctx, expr->result, "object.%d");
	struct qbe_value base = mkqval(ctx, &out);
	struct qbe_value sz = constl(expr->result->size);
	enum qbe_instr alloc = alloc_for_align(expr->result->align);
	pushprei(ctx->current, &base, alloc, &sz, NULL);
	gen_expr_at(ctx, expr, out);
	return out;
}

static void
gen_expr_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out)
{
	assert(out.kind != GV_CONST);

	switch (expr->type) {
	case EXPR_ALLOC:
		gen_expr_alloc_with(ctx, expr, &out);
		return;
	case EXPR_CAST:
		gen_expr_cast_at(ctx, expr, out);
		return;
	case EXPR_COMPOUND:
		gen_expr_compound_with(ctx, expr, &out);
		return;
	case EXPR_CONSTANT:
		gen_expr_const_at(ctx, expr, out);
		return;
	case EXPR_IF:
		gen_expr_if_with(ctx, expr, &out);
		return;
	case EXPR_MATCH:
		gen_expr_match_with(ctx, expr, &out);
		return;
	case EXPR_SLICE:
		gen_expr_slice_at(ctx, expr, out);
		return;
	case EXPR_STRUCT:
		gen_expr_struct_at(ctx, expr, out);
		return;
	case EXPR_SWITCH:
		gen_expr_switch_with(ctx, expr, &out);
		return;
	case EXPR_TUPLE:
		gen_expr_tuple_at(ctx, expr, out);
		return;
	case EXPR_VASTART:
		gen_expr_vastart_at(ctx, expr, out);
		return;
	default:
		break; // Prefers non-at style
	}

	struct gen_value result = gen_expr(ctx, expr);
	if (!expr->terminates) {
		gen_store(ctx, out, result);
	}
}

static struct gen_value
gen_expr_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	if (out) {
		gen_expr_at(ctx, expr, *out);
		return *out;
	}
	return gen_expr(ctx, expr);
}

static struct qbe_data_item *gen_data_item(struct gen_context *,
	struct expression *, struct qbe_data_item *);

static void
gen_function_decl(struct gen_context *ctx, const struct declaration *decl)
{
	const struct function_decl *func = &decl->func;
	const struct type *fntype = func->type;
	if (func->body == NULL) {
		return; // Prototype
	}

	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_FUNC;
	qdef->exported = decl->exported;
	ctx->current = &qdef->func;

	qdef->name = decl->symbol ? xstrdup(decl->symbol)
		: ident_to_sym(&decl->ident);
	qdef->file = decl->loc.file;

	struct qbe_statement start_label = {0};
	mklabel(ctx, &start_label, "start.%d");
	push(&qdef->func.prelude, &start_label);

	if (type_dealias(NULL, fntype->func.result)->storage != STORAGE_VOID) {
		qdef->func.returns = qtype_lookup(
			ctx, fntype->func.result, false);
	} else {
		qdef->func.returns = &qbe_void;
	}
	if (fntype->func.variadism == VARIADISM_C) {
		qdef->func.variadic = true;
	}

	struct qbe_func_param *param, **next = &qdef->func.params;
	for (struct scope_object *obj = decl->func.scope->objects;
			obj; obj = obj->lnext) {
		const struct type *type = obj->type;
		param = *next = xcalloc(1, sizeof(struct qbe_func_param));
		assert(!obj->ident.ns); // Invariant
		param->name = xstrdup(obj->ident.name);
		param->type = qtype_lookup(ctx, type, false);

		struct gen_binding *gb =
			xcalloc(1, sizeof(struct gen_binding));
		gb->value.kind = GV_TEMP;
		gb->value.type = type;
		gb->object = obj;
		if (type_is_aggregate(type)) {
			// No need to copy to stack
			gb->value.name = xstrdup(param->name);
		} else {
			gb->value.name = gen_name(&ctx->id, "param.%d");

			struct qbe_value qv = mklval(ctx, &gb->value);
			struct qbe_value sz = constl(type->size);
			enum qbe_instr alloc = alloc_for_align(type->align);
			pushprei(ctx->current, &qv, alloc, &sz, NULL);
			struct gen_value src = {
				.kind = GV_TEMP,
				.type = type,
				.name = param->name,
			};
			gen_store(ctx, gb->value, src);
		}

		gb->next = ctx->bindings;
		ctx->bindings = gb;
		next = &param->next;
	}

	struct qbe_statement lbody;
	mklabel(ctx, &lbody, "body.%d");
	push(&ctx->current->body, &lbody);
	struct gen_value ret = gen_expr(ctx, decl->func.body);

	if (fntype->func.flags & FN_NORETURN) {
		gen_fixed_abort(ctx, decl->loc, ABORT_NORETURN);
	} else if (decl->func.body->terminates) {
		// XXX: This is a bit hacky, to appease qbe
		size_t ln = ctx->current->body.ln;
		struct qbe_statement *last = &ctx->current->body.stmts[ln - 1];
		if (last->type != Q_INSTR || last->instr != Q_RET) {
			pushi(ctx->current, NULL, Q_RET, NULL);
		}
	} else if (type_dealias(NULL, fntype->func.result)->storage != STORAGE_VOID) {
		struct qbe_value qret = mkqval(ctx, &ret);
		pushi(ctx->current, NULL, Q_RET, &qret, NULL);
	} else {
		pushi(ctx->current, NULL, Q_RET, NULL);
	}

	qbe_append_def(ctx->out, qdef);

	if (func->flags & FN_INIT) {
		struct qbe_def *init = xcalloc(1, sizeof *init);
		init->kind = Q_DATA;
		init->exported = false;
		init->data.align = 8;
		init->data.section = ".init_array";
		init->data.secflags = NULL;

		size_t n = snprintf(NULL, 0, ".init.%s", qdef->name);
		init->name = xcalloc(n + 1, 1);
		snprintf(init->name, n + 1, ".init.%s", qdef->name);

		struct qbe_data_item dataitem = {
			.type = QD_VALUE,
			.value = {
				.kind = QV_GLOBAL,
				.type = &qbe_long,
				.name = xstrdup(qdef->name),
			},
			.next = NULL,
		};
		init->data.items = dataitem;

		qbe_append_def(ctx->out, init);
	}

	if (func->flags & FN_FINI) {
		struct qbe_def *fini = xcalloc(1, sizeof *fini);
		fini->kind = Q_DATA;
		fini->exported = false;
		fini->data.align = 8;
		fini->data.section = ".fini_array";
		fini->data.secflags = NULL;

		size_t n = snprintf(NULL, 0, ".fini.%s", qdef->name);
		fini->name = xcalloc(n + 1, 1);
		snprintf(fini->name, n + 1, ".fini.%s", qdef->name);

		struct qbe_data_item dataitem = {
			.type = QD_VALUE,
			.value = {
				.kind = QV_GLOBAL,
				.type = &qbe_long,
				.name = xstrdup(qdef->name),
			},
			.next = NULL,
		};
		fini->data.items = dataitem;

		qbe_append_def(ctx->out, fini);
	}

	if (func->flags & FN_TEST) {
		struct qbe_def *test = xcalloc(1, sizeof *test);
		test->kind = Q_DATA;
		test->exported = false;
		test->data.align = 8;
		test->data.section = ".test_array";
		test->data.secflags = "aw";

		size_t n = snprintf(NULL, 0, ".test.%s", qdef->name);
		test->name = xcalloc(n + 1, 1);
		snprintf(test->name, n + 1, ".test.%s", qdef->name);

		char *ident = identifier_unparse(&decl->ident);

		struct qbe_data_item *dataitem = &test->data.items;
		struct expression expr;
		mkstrconst(&expr, "%s", ident);
		dataitem = gen_data_item(ctx, &expr, dataitem);

		struct qbe_data_item *next = xcalloc(1, sizeof *next);
		next->type = QD_VALUE;
		next->value.kind = QV_GLOBAL;
		next->value.type = &qbe_long;
		next->value.name = xstrdup(qdef->name);
		next->next = NULL;
		dataitem->next = next;

		qbe_append_def(ctx->out, test);
	}

	ctx->current = NULL;
}

static struct qbe_data_item *
gen_data_item(struct gen_context *ctx, struct expression *expr,
	struct qbe_data_item *item)
{
	assert(expr->type == EXPR_CONSTANT);

	struct qbe_def *def;
	const struct expression_constant *constant = &expr->constant;
	const struct type *type = type_dealias(NULL, expr->result);
	if (type->storage == STORAGE_ENUM) {
		type = type->alias.type;
	}
	type = lower_const(NULL, type, NULL);
	if (constant->object) {
		item->type = QD_SYMOFFS;
		item->sym = ident_to_sym(&constant->object->ident);
		item->offset = constant->ival;
		return item;
	}

	switch (type->storage) {
	case STORAGE_I8:
	case STORAGE_U8:
		item->type = QD_VALUE;
		item->value = constw((uint8_t)constant->uval);
		item->value.type = &qbe_byte;
		break;
	case STORAGE_BOOL:
		item->type = QD_VALUE;
		item->value = constw(constant->bval ? 1 : 0);
		item->value.type = &qbe_byte;
		break;
	case STORAGE_I16:
	case STORAGE_U16:
		item->type = QD_VALUE;
		item->value = constw((uint16_t)constant->uval);
		item->value.type = &qbe_half;
		break;
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_RUNE:
		item->type = QD_VALUE;
		item->value = constw((uint32_t)constant->uval);
		break;
	case STORAGE_U64:
	case STORAGE_I64:
	case STORAGE_SIZE:
		item->type = QD_VALUE;
		item->value = constl((uint64_t)constant->uval);
		break;
	case STORAGE_F32:
		item->type = QD_VALUE;
		item->value = consts((float)constant->fval);
		break;
	case STORAGE_F64:
		item->type = QD_VALUE;
		item->value = constd((double)constant->fval);
		break;
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
		item->type = QD_VALUE;
		switch (ctx->arch.ptr->stype) {
		case Q_LONG:
			item->value = constl((uint64_t)constant->uval);
			break;
		default: assert(0);
		}
		break;
	case STORAGE_ARRAY:
		assert(type->array.length != SIZE_UNDEFINED);
		size_t n = type->array.length;
		for (struct array_constant *c = constant->array;
				c && n; c = c->next ? c->next : c, --n) {
			item = gen_data_item(ctx, c->value, item);
			if (n > 1 || c->next) {
				item->next = xcalloc(1,
					sizeof(struct qbe_data_item));
				item = item->next;
			}
		}
		break;
	case STORAGE_STRING:
		def = xcalloc(1, sizeof(struct qbe_def));
		def->name = gen_name(&ctx->id, "strdata.%d");
		def->kind = Q_DATA;
		def->data.align = ALIGN_UNDEFINED;
		def->data.items.type = QD_STRING;
		def->data.items.str = xcalloc(expr->constant.string.len, 1);
		def->data.items.sz = expr->constant.string.len;
		memcpy(def->data.items.str, expr->constant.string.value,
			expr->constant.string.len);

		item->type = QD_VALUE;
		if (expr->constant.string.len != 0) {
			qbe_append_def(ctx->out, def);
			item->value.kind = QV_GLOBAL;
			item->value.type = &qbe_long;
			item->value.name = xstrdup(def->name);
		} else {
			free(def);
			item->value = constl(0);
		}

		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		item->value = constl(expr->constant.string.len);
		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		item->value = constl(expr->constant.string.len);
		break;
	case STORAGE_SLICE:
		def = xcalloc(1, sizeof(struct qbe_def));
		def->name = gen_name(&ctx->id, "sldata.%d");
		def->kind = Q_DATA;
		def->data.align = ALIGN_UNDEFINED;

		size_t len = 0;
		struct qbe_data_item *subitem = &def->data.items;
		for (struct array_constant *c = constant->array;
				c; c = c->next) {
			subitem = gen_data_item(ctx, c->value, subitem);
			if (c->next) {
				subitem->next = xcalloc(1,
					sizeof(struct qbe_data_item));
				subitem = subitem->next;
			}
			++len;
		}

		item->type = QD_VALUE;
		if (len != 0) {
			qbe_append_def(ctx->out, def);
			item->value.kind = QV_GLOBAL;
			item->value.type = &qbe_long;
			item->value.name = xstrdup(def->name);
		} else {
			free(def);
			item->value = constl(0);
		}

		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		item->value = constl(len);
		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		item->value = constl(len);
		break;
	case STORAGE_STRUCT:
		for (struct struct_constant *f = constant->_struct;
				f; f = f->next) {
			item = gen_data_item(ctx, f->value, item);
			if (f->next) {
				const struct struct_field *f1 = f->field;
				const struct struct_field *f2 = f->next->field;
				if (f2->offset != f1->offset + f1->type->size) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
					item->type = QD_ZEROED;
					item->zeroed = f2->offset -
						(f1->offset + f1->type->size);
				}

				item->next = xcalloc(1,
					sizeof(struct qbe_data_item));
				item = item->next;
			} else {
				const struct struct_field *fi = f->field;
				if (fi->offset + fi->type->size
						!= expr->result->size) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
					item->type = QD_ZEROED;
					item->zeroed = expr->result->size
						- (fi->offset + fi->type->size);
				}
			}
		}
		break;
	case STORAGE_TUPLE:
		for (const struct tuple_constant *tuple = constant->tuple;
				tuple; tuple = tuple->next) {
			item = gen_data_item(ctx, tuple->value, item);
			if (tuple->next) {
				const struct type_tuple *f1 = tuple->field;
				const struct type_tuple *f2 = tuple->next->field;
				if (f2->offset != f1->offset + f1->type->size) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
					item->type = QD_ZEROED;
					item->zeroed = f2->offset -
						(f1->offset + f1->type->size);
				}


				item->next = xcalloc(1,
					sizeof(struct qbe_data_item));
				item = item->next;
			} else {
				const struct type_tuple *fi = tuple->field;
				if (fi->offset + fi->type->size
						!= expr->result->size) {
					item->next = xcalloc(1,
						sizeof(struct qbe_data_item));
					item = item->next;
					item->type = QD_ZEROED;
					item->zeroed = expr->result->size
						- (fi->offset + fi->type->size);
				}
			}
		}
		break;
	case STORAGE_TAGGED:
		item->type = QD_VALUE;
		item->value = constw((uint32_t)constant->tagged.tag->id);
		if (type->align != builtin_type_uint.align) {
			item->next = xcalloc(1, sizeof(struct qbe_data_item));
			item = item->next;
			item->type = QD_ZEROED;
			item->zeroed = type->align - builtin_type_uint.align;
		}
		if (constant->tagged.tag->size != 0) {
			item->next = xcalloc(1, sizeof(struct qbe_data_item));
			item = item->next;
			item = gen_data_item(ctx, constant->tagged.value, item);
		}
		if (constant->tagged.tag->size < type->size - type->align) {
			item->next = xcalloc(1, sizeof(struct qbe_data_item));
			item = item->next;
			item->type = QD_ZEROED;
			item->zeroed = type->size - type->align - constant->tagged.tag->size;
		}
		break;
	case STORAGE_ENUM:
	case STORAGE_UNION:
	case STORAGE_ALIAS:
	case STORAGE_ERROR:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_OPAQUE:
	case STORAGE_RCONST:
	case STORAGE_NULL:
	case STORAGE_VALIST:
	case STORAGE_VOID:
		assert(0); // Invariant
	}

	assert(item->type != QD_VALUE || item->value.type);
	return item;
}

static void
gen_global_decl(struct gen_context *ctx, const struct declaration *decl)
{
	assert(decl->decl_type == DECL_GLOBAL);
	const struct global_decl *global = &decl->global;
	if (!global->value) {
		return; // Forward declaration
	}
	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_DATA;
	qdef->data.align = ALIGN_UNDEFINED;
	qdef->data.threadlocal = global->threadlocal;
	qdef->exported = decl->exported;
	qdef->name = ident_to_sym(&decl->ident);
	qdef->file = decl->loc.file;
	gen_data_item(ctx, global->value, &qdef->data.items);
	qbe_append_def(ctx->out, qdef);
}

static void
gen_decl(struct gen_context *ctx, const struct declaration *decl)
{
	switch (decl->decl_type) {
	case DECL_FUNC:
		gen_function_decl(ctx, decl);
		break;
	case DECL_GLOBAL:
		gen_global_decl(ctx, decl);
		break;
	case DECL_TYPE:
	case DECL_CONST:
		break; // no-op
	}
}

void
gen(const struct unit *unit, struct type_store *store, struct qbe_program *out)
{
	struct gen_context ctx = {
		.out = out,
		.store = store,
		.ns = unit->ns,
		.arch = {
			.ptr = &qbe_long,
			.sz = &qbe_long,
		},
	};
	ctx.out->next = &ctx.out->defs;
	rtfunc_init(&ctx);
	const struct declarations *decls = unit->declarations;
	while (decls) {
		gen_decl(&ctx, &decls->decl);
		decls = decls->next;
	}
}
