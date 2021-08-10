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

static void
gen_defers(struct gen_context *ctx, struct gen_scope *scope)
{
	if (!scope) {
		return;
	}
	if (scope->defers) {
		pushc(ctx->current, "gen defers");
	}
	for (struct gen_defer *defer = scope->defers; defer;
			defer = defer->next) {
		gen_expr(ctx, defer->expr);
	}
}

static void
push_scope(struct gen_context *ctx)
{
	struct gen_scope *scope = xcalloc(1, sizeof(struct gen_scope));
	scope->parent = ctx->scope;
	ctx->scope = scope;
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
	struct qbe_value rtfunc = mkrtfunc(ctx, "rt.memcpy");
	struct qbe_value dtemp = mklval(ctx, &dest);
	struct qbe_value stemp = mklval(ctx, &src);
	struct qbe_value sz = constl(dest.type->size);
	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &dtemp, &stemp, &sz, NULL);
}

static void
gen_copy_aligned(struct gen_context *ctx,
	struct gen_value dest, struct gen_value src)
{
	if (dest.type->size > 128) {
		gen_copy_memcpy(ctx, dest, src);
		return;
	}
	enum qbe_instr load, store;
	assert(dest.type->align && (dest.type->align & (dest.type->align - 1)) == 0);
	switch (dest.type->align) {
	case 1: load = Q_LOADUB, store = Q_STOREB; break;
	case 2: load = Q_LOADUH, store = Q_STOREH; break;
	case 4: load = Q_LOADUW, store = Q_STOREW; break;
	default:
		assert(dest.type->align == 8);
		load = Q_LOADL, store = Q_STOREL;
		break;
	}
	struct qbe_value temp = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = gen_name(ctx, ".%d"),
	};
	struct qbe_value destp = mkcopy(ctx, &dest, ".%d");
	struct qbe_value srcp = mkcopy(ctx, &src, ".%d");
	struct qbe_value align = constl(dest.type->align);
	for (size_t offset = 0; offset < dest.type->size;
			offset += dest.type->align) {
		pushi(ctx->current, &temp, load, &srcp, NULL);
		pushi(ctx->current, NULL, store, &temp, &destp, NULL);
		if (offset + dest.type->align < dest.type->size) {
			pushi(ctx->current, &srcp, Q_ADD, &srcp, &align, NULL);
			pushi(ctx->current, &destp, Q_ADD, &destp, &align, NULL);
		}
	}
}

static void
gen_store(struct gen_context *ctx,
	struct gen_value object,
	struct gen_value value)
{
	const struct type *ty = type_dealias(object.type);
	switch (ty->storage) {
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		gen_copy_aligned(ctx, object, value);
		return;
	case STORAGE_UNION:
		gen_copy_memcpy(ctx, object, value);
		return;
	case STORAGE_ENUM:
		object.type = builtin_type_for_storage(ty->_enum.storage,
			(ty->flags & TYPE_CONST) != 0);
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
	const struct type *ty = type_dealias(object.type);
	switch (ty->storage) {
	case STORAGE_ARRAY:
	case STORAGE_FUNCTION:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
		return object;
	case STORAGE_ENUM:
		object.type = builtin_type_for_storage(ty->_enum.storage,
			(ty->flags & TYPE_CONST) != 0);
		break;
	default:
		break; // no-op
	}

	struct gen_value value = {
		.kind = GV_TEMP,
		.type = object.type,
		.name = gen_name(ctx, "load.%d"),
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
	int n = snprintf(NULL, 0, "%s:%d:%d", loc.path, loc.lineno, loc.colno);
	char *s = xcalloc(1, n + 1);
	snprintf(s, n, "%s:%d:%d", loc.path, loc.lineno, loc.colno);
	struct expression eloc = {0};
	eloc.type = EXPR_CONSTANT;
	eloc.result = &builtin_type_const_str;
	eloc.constant.string.value = s;
	eloc.constant.string.len = n - 1;
	struct gen_value msg = gen_expr(ctx, &eloc);
	struct qbe_value qmsg = mkqval(ctx, &msg);
	struct qbe_value rtabort = mkrtfunc(ctx, "rt.abort_fixed");
	struct qbe_value tmp = constl(reason);
	pushi(ctx->current, NULL, Q_CALL, &rtabort, &qmsg, &tmp, NULL);
}

static struct gen_value
gen_autoderef(struct gen_context *ctx, struct gen_value val)
{
	while (type_dealias(val.type)->storage == STORAGE_POINTER) {
		val.type = type_dealias(val.type)->pointer.referent;
		val = gen_load(ctx, val);
	}
	return val;
}

static struct gen_value
gen_access_ident(struct gen_context *ctx, const struct expression *expr)
{
	const struct scope_object *obj = expr->access.object;
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
		};
	case O_CONST:
	case O_TYPE:
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
	bool checkbounds = true;
	struct qbe_value length;
	const struct type *ty = type_dealias(glval.type);
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
		addr = gen_access_ident(ctx, expr);
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
		struct gen_value out)
{
	struct qbe_value qcap;
	if (expr->alloc.cap) {
		struct gen_value cap = gen_expr(ctx, expr->alloc.cap);
		qcap = mkqval(ctx, &cap);
	}

	struct gen_value init;
	struct qbe_value qinit;
	struct qbe_value length, initdata;
	const struct type *inittype = type_dealias(expr->alloc.expr->result);
	switch (inittype->storage) {
	case STORAGE_ARRAY:
		assert(inittype->array.length != SIZE_UNDEFINED);
		length = constl(inittype->array.length);
		break;
	case STORAGE_SLICE:
		init = gen_expr(ctx, expr->alloc.expr);
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

	const struct type *sltype = type_dealias(expr->result);
	struct qbe_value isize = constl(sltype->array.members->size);
	struct qbe_value size = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &size, Q_MUL, &qcap, &isize, NULL);

	struct qbe_statement lzero, lnonzero;
	struct qbe_value bzero = mklabel(ctx, &lzero, ".%d");
	struct qbe_value bnonzero = mklabel(ctx, &lnonzero, ".%d");

	struct qbe_value rtfunc = mkrtfunc(ctx, "rt.malloc");
	struct qbe_value data = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value zero = constl(0);
	pushi(ctx->current, &data, Q_COPY, &zero, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &size, &bnonzero, &bzero, NULL);
	push(&ctx->current->body, &lnonzero);
	pushi(ctx->current, &data, Q_CALL, &rtfunc, &size, NULL);

	struct qbe_statement linvalid;
	struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
	pushi(ctx->current, NULL, Q_JNZ, &data, &bzero, &binvalid, NULL);
	push(&ctx->current->body, &linvalid);
	gen_fixed_abort(ctx, expr->loc, ABORT_ALLOC_FAILURE);
	push(&ctx->current->body, &lzero);

	struct qbe_value base = mklval(ctx, &out);
	struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value offset = constl(builtin_type_size.size);
	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, NULL, store, &data, &base, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &base, &offset, NULL);
	pushi(ctx->current, NULL, store, &length, &ptr, NULL);
	offset = constl(builtin_type_size.size * 2);
	pushi(ctx->current, &ptr, Q_ADD, &base, &offset, NULL);
	pushi(ctx->current, NULL, store, &qcap, &ptr, NULL);

	if (inittype->storage == STORAGE_ARRAY) {
		struct gen_value storage = (struct gen_value){
			.kind = GV_TEMP,
			.type = inittype,
			.name = data.name,
		};
		gen_expr_at(ctx, expr->alloc.expr, storage);
	} else {
		struct qbe_value rtmemcpy = mkrtfunc(ctx, "rt.memcpy");
		pushi(ctx->current, NULL, Q_CALL, &rtmemcpy,
				&data, &initdata, &size, NULL);
	}
}

static struct gen_value
gen_expr_alloc_with(struct gen_context *ctx,
	const struct expression *expr, struct gen_value *out)
{
	if (type_dealias(expr->result)->storage == STORAGE_SLICE) {
		if (out) {
			gen_alloc_slice_at(ctx, expr, *out);
			return gv_void;
		}
		struct gen_value temp = mktemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mkqval(ctx, &temp);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_alloc_slice_at(ctx, expr, temp);
		return temp;
	}
	assert(expr->alloc.cap == NULL);

	struct qbe_value sz = constl(type_dereference(expr->result)->size);
	struct gen_value result = mktemp(ctx, expr->result, ".%d");
	struct qbe_value qresult = mkqval(ctx, &result);
	struct qbe_value rtfunc = mkrtfunc(ctx, "rt.malloc");
	pushi(ctx->current, &qresult, Q_CALL, &rtfunc, &sz, NULL);

	if (!(type_dealias(expr->result)->pointer.flags & PTR_NULLABLE)) {
		struct qbe_statement linvalid, lvalid;
		struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
		struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");

		pushi(ctx->current, NULL, Q_JNZ, &qresult, &bvalid, &binvalid, NULL);
		push(&ctx->current->body, &linvalid);
		gen_fixed_abort(ctx, expr->loc, ABORT_ALLOC_FAILURE);
		push(&ctx->current->body, &lvalid);
	}

	gen_expr_at(ctx, expr->alloc.expr, result);
	if (out) {
		gen_store(ctx, *out, result);
	}
	return result;
}

static struct gen_value
gen_expr_append(struct gen_context *ctx, const struct expression *expr)
{
	assert(!expr->append.is_static); // TODO

	struct gen_value slice = gen_expr(ctx, expr->append.expr);
	struct qbe_value qslice = mkqval(ctx, &slice);

	struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value offs = constl(builtin_type_size.size);
	pushi(ctx->current, &ptr, Q_ADD, &qslice, &offs, NULL);
	struct qbe_value len = mkqtmp(ctx, ctx->arch.sz, ".%d");
	enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, &len, load, &ptr, NULL);

	size_t args = 0;
	for (struct append_values *value = expr->append.values; value;
			value = value->next) {
		args++;
	}
	struct qbe_value qargs = constl(args);
	struct qbe_value newlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &newlen, Q_ADD, &len, &qargs, NULL);

	const struct type *vtype = NULL;
	struct qbe_value vdata, vlen;
	if (expr->append.variadic) {
		// TODO: If it's an array object we might be able to make this
		// more efficient by using gen_expr_at to populate the expanded
		// slice storage area with the variadic data, like
		// gen_alloc_slice_at does.
		struct gen_value vobj = gen_expr(ctx, expr->append.variadic);
		struct qbe_value qvobj = mkqval(ctx, &vobj);
		struct qbe_value ptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		vdata = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		vtype = type_dealias(expr->append.variadic->result);
		switch (vtype->storage) {
		case STORAGE_ARRAY:
			pushi(ctx->current, &vdata, Q_COPY, &qvobj, NULL);
			vlen = constl(vtype->array.length);
			break;
		case STORAGE_SLICE:
			vlen = mkqtmp(ctx, ctx->arch.sz, ".%d");
			pushi(ctx->current, &vdata, load, &qvobj, NULL);
			pushi(ctx->current, &ptr, Q_ADD, &qvobj, &offs, NULL);
			pushi(ctx->current, &vlen, load, &ptr, NULL);
			break;
		default: abort(); // Invariant
		}

		pushi(ctx->current, &newlen, Q_ADD, &newlen, &vlen, NULL);
	}

	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, NULL, store, &newlen, &ptr, NULL);

	const struct type *mtype = type_dealias(slice.type)->array.members;
	struct qbe_value membsz = constl(mtype->size);
	struct qbe_value rtfunc = mkrtfunc(ctx, "rt.ensure");
	struct qbe_value lval = mklval(ctx, &slice);
	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &lval, &membsz, NULL);

	offs = mkqtmp(ctx, ctx->arch.sz, ".%d");
	pushi(ctx->current, &ptr, load, &qslice, NULL);
	pushi(ctx->current, &offs, Q_MUL, &len, &membsz, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &ptr, &offs, NULL);

	struct gen_value item = {
		.kind = GV_TEMP,
		.type = mtype,
		.name = ptr.name,
	};
	for (struct append_values *value = expr->append.values; value;
			value = value->next) {
		gen_expr_at(ctx, value->expr, item);
		pushi(ctx->current, &ptr, Q_ADD, &ptr, &membsz, NULL);
	}

	if (expr->append.variadic) {
		struct qbe_value rtfunc = mkrtfunc(ctx, "rt.memcpy");
		struct qbe_value sz = mkqtmp(ctx, ctx->arch.sz, ".%d");
		pushi(ctx->current, &sz, Q_MUL, &vlen, &membsz, NULL);
		pushi(ctx->current, NULL, Q_CALL, &rtfunc, &ptr, &vdata, &sz, NULL);
	}

	return gv_void;
}

static struct gen_value
gen_expr_assert(struct gen_context *ctx, const struct expression *expr)
{
	assert(expr->assert.message); // Invariant
	if (expr->assert.is_static) {
		return gv_void;
	}

	struct gen_value msg;
	struct qbe_statement failedl, passedl;
	struct qbe_value rtfunc = mkrtfunc(ctx, "rt.abort");
	if (expr->assert.cond) {
		struct qbe_value bfailed = mklabel(ctx, &failedl, "failed.%d");
		struct qbe_value bpassed = mklabel(ctx, &passedl, "passed.%d");
		struct gen_value cond = gen_expr(ctx, expr->assert.cond);
		struct qbe_value qcond = mkqval(ctx, &cond);
		pushi(ctx->current, NULL, Q_JNZ, &qcond, &bpassed, &bfailed, NULL);
		push(&ctx->current->body, &failedl);
		msg = gen_expr(ctx, expr->assert.message);
	} else {
		msg = gen_expr(ctx, expr->assert.message);
	}

	struct qbe_value qmsg = mkqval(ctx, &msg);
	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &qmsg, NULL);

	if (expr->assert.cond) {
		push(&ctx->current->body, &passedl);
	}

	return gv_void;
}

static struct gen_value
gen_expr_assign_slice(struct gen_context *ctx, const struct expression *expr)
{
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
	pushi(ctx->current, &tmp, Q_CUGEL, &olen, &vlen, NULL);
	pushi(ctx->current, NULL, Q_JNZ, &tmp, &bvalid, &binvalid, NULL);
	push(&ctx->current->body, &linvalid);
	gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
	push(&ctx->current->body, &lvalid);

	struct qbe_value rtmemcpy = mkrtfunc(ctx, "rt.memcpy");
	struct qbe_value optr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	struct qbe_value vptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	pushi(ctx->current, &optr, Q_LOADL, &qobj, NULL);
	pushi(ctx->current, &vptr, Q_LOADL, &qval, NULL);
	tmp = constl(expr->assign.object->result->array.members->size);
	pushi(ctx->current, &olen, Q_MUL, &olen, &tmp, NULL);
	pushi(ctx->current, NULL, Q_CALL, &rtmemcpy, &optr, &vptr, &olen, NULL);

	return gv_void;
}

static struct gen_value
gen_expr_assign(struct gen_context *ctx, const struct expression *expr)
{
	struct expression *object = expr->assign.object;
	struct expression *value = expr->assign.value;
	if (object->type == EXPR_SLICE) {
		return gen_expr_assign_slice(ctx, expr);
	}
	assert(object->type == EXPR_ACCESS || expr->assign.indirect); // Invariant

	struct gen_value obj;
	if (expr->assign.indirect) {
		obj = gen_expr(ctx, object);
		obj.type = type_dealias(object->result)->pointer.referent;
	} else {
		obj = gen_expr_access_addr(ctx, object);
	}
	if (expr->assign.op == BIN_LEQUAL) {
		gen_expr_at(ctx, value, obj);
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
		struct qbe_value qrval = mkqval(ctx, &rvalue);
		enum qbe_instr instr = binarithm_for_op(ctx,
			expr->assign.op, lvalue.type);
		pushi(ctx->current, &qlval, instr, &qlval, &qrval, NULL);
		gen_store(ctx, obj, lvalue);
	}

	return gv_void;
}

static struct qbe_value
extend(struct gen_context *ctx, struct qbe_value v, const struct type *type)
{
	enum qbe_instr op;
	switch (type->size) {
	case 1:
		op = type_is_signed(type) ? Q_EXTSB : Q_EXTUB;
		break;
	case 2:
		op = type_is_signed(type) ? Q_EXTSH : Q_EXTUH;
		break;
	default:
		return v;
	}

	struct qbe_value temp = mkqtmp(ctx, &qbe_word, "ext.%d");
	pushi(ctx->current, &temp, op, &v, NULL);
	return temp;
}

static struct gen_value
gen_expr_binarithm(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *ltype = type_dealias(expr->binarithm.lvalue->result);
	const struct type *rtype = type_dealias(expr->binarithm.rvalue->result);
	struct gen_value result = mktemp(ctx, expr->result, ".%d");
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

	switch (expr->binarithm.op) {
	case BIN_GREATER:
	case BIN_GREATEREQ:
	case BIN_LEQUAL:
	case BIN_LESS:
	case BIN_LESSEQ:
	case BIN_NEQUAL:
		qlval = extend(ctx, qlval, ltype);
		qrval = extend(ctx, qrval, rtype);
		break;
	default:
		break;
	}

	assert((ltype->storage == STORAGE_STRING) == (rtype->storage == STORAGE_STRING));
	if (ltype->storage == STORAGE_STRING) {
		struct qbe_value rtfunc = mkrtfunc(ctx, "rt.strcmp");
		pushi(ctx->current, &qresult, Q_CALL,
			&rtfunc, &qlval, &qrval, NULL);
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

static struct gen_value
gen_expr_binding(struct gen_context *ctx, const struct expression *expr)
{
	for (const struct expression_binding *binding = &expr->binding;
			binding; binding = binding->next) {
		if (binding->object->otype == O_DECL) {
			// static binding
			struct declaration decl = {
				.type = DECL_GLOBAL,
				.ident = binding->object->ident,
				.global = {
					.type = binding->object->type,
					.value = binding->initializer,
				},
			};
			gen_global_decl(ctx, &decl);
			continue;
		}

		const struct type *type = binding->initializer->result;
		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value.kind = GV_TEMP;
		gb->value.type = type;
		gb->value.name = gen_name(ctx, "binding.%d");
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
	struct gen_scope *scope = ctx->scope;
	while (scope != NULL) {
		gen_defers(ctx, scope);
		if (expr->control.label && scope->label) {
			if (strcmp(expr->control.label, scope->label) == 0) {
				break;
			}
		} else if (!expr->control.label && scope->after) {
			break;
		}
		scope = scope->parent;
	}
	assert(scope != NULL);
	if (expr->type == EXPR_BREAK) {
		pushi(ctx->current, NULL, Q_JMP, scope->end, NULL);
	} else {
		pushi(ctx->current, NULL, Q_JMP, scope->after, NULL);
	}
	return gv_void;
}

static struct gen_value
gen_expr_call(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value lvalue = gen_expr(ctx, expr->call.lvalue);
	lvalue = gen_autoderef(ctx, lvalue);

	const struct type *rtype = lvalue.type;
	assert(rtype->storage == STORAGE_FUNCTION);

	if (rtype->func.flags & FN_NORETURN) {
		for (struct gen_scope *scope = ctx->scope; scope;
				scope = scope->parent) {
			gen_defers(ctx, scope);
		}
	}

	struct qbe_statement call = {
		.type = Q_INSTR,
		.instr = Q_CALL,
	};
	struct gen_value rval = gv_void;
	if (type_dealias(rtype->func.result)->storage != STORAGE_VOID) {
		rval = mktemp(ctx, rtype->func.result, "returns.%d");
		call.out = xcalloc(1, sizeof(struct qbe_value));
		*call.out = mkqval(ctx, &rval);
	}

	struct qbe_arguments *args, **next = &call.args;
	args = *next = xcalloc(1, sizeof(struct qbe_arguments));
	args->value = mkqval(ctx, &lvalue);
	next = &args->next;
	for (struct call_argument *carg = expr->call.args;
			carg; carg = carg->next) {
		args = *next = xcalloc(1, sizeof(struct qbe_arguments));
		struct gen_value arg = gen_expr(ctx, carg->value);
		args->value = mkqval(ctx, &arg);
		next = &args->next;
	}
	push(&ctx->current->body, &call);

	return rval;
}

static struct gen_value gen_expr_cast(struct gen_context *ctx,
		const struct expression *expr);

static struct gen_value
gen_expr_type_test(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *secondary = expr->cast.secondary,
	      *from = expr->cast.value->result;
	assert(type_dealias(from)->storage == STORAGE_TAGGED);
	const struct type *subtype = tagged_select_subtype(from, secondary);
	assert(subtype && subtype == secondary); // Lowered by check

	struct gen_value val = gen_expr(ctx, expr->cast.value);
	struct qbe_value qval = mkqval(ctx, &val);
	struct qbe_value tag = mkqtmp(ctx,
			qtype_lookup(ctx, &builtin_type_uint, false),
			".%d");
	enum qbe_instr load = load_for_type(ctx, &builtin_type_uint);
	pushi(ctx->current, &tag, load, &qval, NULL);
	struct qbe_value expected = constl(secondary->id);
	struct gen_value result = mktemp(ctx, &builtin_type_bool, ".%d");
	struct qbe_value qr = mkqval(ctx, &result);
	pushi(ctx->current, &qr, Q_CEQW, &tag, &expected, NULL);
	return result;
}

static void
gen_type_assertion(struct gen_context *ctx,
		const struct expression *expr,
		struct qbe_value base)
{
	const struct type *want = expr->result;
	struct qbe_value tag = mkqtmp(ctx,
		qtype_lookup(ctx, &builtin_type_uint, false),
		".%d");
	enum qbe_instr load = load_for_type(ctx, &builtin_type_uint);
	pushi(ctx->current, &tag, load, &base, NULL);
	struct qbe_value expected = constl(want->id);
	struct gen_value result = mktemp(ctx, &builtin_type_bool, ".%d");
	struct qbe_value qr = mkqval(ctx, &result);
	pushi(ctx->current, &qr, Q_CEQW, &tag, &expected, NULL);

	struct qbe_statement failedl, passedl;
	struct qbe_value bfailed = mklabel(ctx, &failedl, "failed.%d");
	struct qbe_value bpassed = mklabel(ctx, &passedl, "passed.%d");
	pushi(ctx->current, NULL, Q_JNZ, &qr, &bpassed, &bfailed, NULL);
	push(&ctx->current->body, &failedl);

	gen_fixed_abort(ctx, expr->loc, ABORT_TYPE_ASSERTION);

	push(&ctx->current->body, &passedl);
}

static void
gen_expr_cast_slice_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	const struct type *to = expr->result, *from = expr->cast.value->result;
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
	assert(type_dealias(object)->storage == STORAGE_TAGGED);
	assert(type_dealias(want)->storage == STORAGE_TAGGED);
	return object->align == want->align
		|| want->size == builtin_type_uint.size;
}

static void
gen_expr_cast_tagged_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	const struct type *to = expr->result, *from = expr->cast.value->result;
	const struct type *subtype = tagged_select_subtype(to, from);
	if ((int)expr->cast.value->type != EXPR_GEN_VALUE) {
		// EXPR_GEN_VALUE is a special case used by match for
		// COMPAT_MISALIGNED. Normally, casts are only allowed between
		// tagged unions if 'to' is a superset of 'from'. However, in
		// the EXPR_GEN_VALUE case, 'to' is a subset of 'from', but the
		// match code has already determined that the selected tag is a
		// member of 'to'.
		assert(subtype || tagged_subset_compat(to, from));
	}

	if (!subtype && tagged_align_compat(from, to)) {
		// Case 1: from is a union whose members are a subset of to, and
		// the alignment matches, so we can just interpret values of
		// type 'from' as if it were of type 'to'
		struct gen_value out2 = out;
		out2.type = from;
		gen_expr_at(ctx, expr->cast.value, out2);
	} else if (!subtype) {
		// Case 2: like case 1, but with an alignment mismatch; more
		// work is required.
		struct gen_value value = gen_expr(ctx, expr->cast.value);
		struct qbe_value qval = mkqval(ctx, &value);
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

		const struct type *innertype = type_store_tagged_to_union(
				ctx->store, type_dealias(from));
		struct gen_value iout = mktemp(ctx, innertype, ".%d");
		struct gen_value ival = mktemp(ctx, innertype, ".%d");
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
			return;
		}

		struct gen_value storage = mktemp(ctx, subtype, ".%d");
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
	// * => tagged
	if (type_dealias(to)->storage == STORAGE_TAGGED) {
		return true;
	}
	// array => slice
	if (type_dealias(from)->storage == STORAGE_ARRAY &&
		type_dealias(to)->storage == STORAGE_SLICE) {
		return true;
	}
	return false;
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

	const struct type *to = expr->result;
	switch (type_dealias(to)->storage) {
	case STORAGE_SLICE:
		gen_expr_cast_slice_at(ctx, expr, out);
		break;
	case STORAGE_TAGGED:
		gen_expr_cast_tagged_at(ctx, expr, out);
		break;
	default: abort(); // Invariant
	}
}

static struct gen_value
gen_expr_cast(struct gen_context *ctx, const struct expression *expr)
{
	const struct type *to = expr->result, *from = expr->cast.value->result;
	switch (expr->cast.kind) {
	case C_TEST:
		return gen_expr_type_test(ctx, expr);
	case C_ASSERTION:
		assert(type_dealias(from)->storage == STORAGE_TAGGED);
		assert(type_dealias(to)->storage != STORAGE_TAGGED);
		// Fallthrough
	case C_CAST:
		break;
	}

	if (cast_prefers_at(expr)) {
		struct gen_value out = mktemp(ctx, expr->result, "object.%d");
		struct qbe_value base = mkqval(ctx, &out);
		struct qbe_value sz = constl(expr->result->size);
		enum qbe_instr alloc = alloc_for_align(expr->result->align);
		pushprei(ctx->current, &base, alloc, &sz, NULL);
		gen_expr_cast_at(ctx, expr, out);
		return out;
	}

	// Special case: tagged => non-tagged
	if (type_dealias(from)->storage == STORAGE_TAGGED) {
		struct gen_value value = gen_expr(ctx, expr->cast.value);
		struct qbe_value base = mkcopy(ctx, &value, ".%d");
		if (expr->cast.kind == C_ASSERTION) {
			gen_type_assertion(ctx, expr, base);
		}

		struct qbe_value align = constl(from->align);
		pushi(ctx->current, &base, Q_ADD, &base, &align, NULL);
		struct gen_value storage = (struct gen_value){
			.kind = GV_TEMP,
			.type = to,
			.name = base.name,
		};
		return gen_load(ctx, storage);
	}

	// Special case: no conversion required
	if (type_dealias(to)->storage == type_dealias(from)->storage
			&& to->size == from->size) {
		struct gen_value value = gen_expr(ctx, expr->cast.value);
		value.type = to;
		return value;
	}

	// Other special cases
	switch (type_dealias(to)->storage) {
	case STORAGE_POINTER:
		if (type_dealias(from)->storage == STORAGE_SLICE) {
			struct gen_value value = gen_expr(ctx, expr->cast.value);
			value.type = to;
			return gen_load(ctx, value);
		}
		break;
	case STORAGE_VOID:
		gen_expr(ctx, expr->cast.value); // Side-effects
		return gv_void;
	default: break;
	}

	struct gen_value value = gen_expr(ctx, expr->cast.value);
	struct qbe_value qvalue = mkqval(ctx, &value);
	struct gen_value result = mktemp(ctx, expr->result, "cast.%d");
	struct qbe_value qresult = mkqval(ctx, &result);

	enum qbe_instr op;
	bool is_signed = type_is_signed(from);
	enum type_storage fstor = type_dealias(from)->storage,
		tstor = type_dealias(to)->storage;
	switch (tstor) {
	case STORAGE_CHAR:
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
		if (type_is_integer(from) && to->size <= from->size) {
			op = Q_COPY;
		} else if (type_is_integer(from) && to->size > from->size) {
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
			assert(tstor == STORAGE_U32);
			op = Q_COPY;
		} else if (type_is_float(from)) {
			if (type_is_signed(to)) {
				switch (fstor) {
				case STORAGE_F32: op = Q_STOSI; break;
				case STORAGE_F64: op = Q_DTOSI; break;
				default: abort(); // Invariant
				}
			} else {
				assert(0); // TODO
			}
		} else {
			abort(); // Invariant
		}
		pushi(ctx->current, &qresult, op, &qvalue, NULL);
		break;
	case STORAGE_F32:
	case STORAGE_F64:
		if (type_is_float(from) && from->size == to->size) {
			op = Q_COPY;
		} else if (type_is_float(from) && to->size < from->size) {
			op = Q_TRUNCD;
		} else if (type_is_float(from) && to->size > from->size) {
			op = Q_EXTS;
		} else if (type_is_integer(from)) {
			if (type_is_signed(from)) {
				switch (from->size) {
				case 4: op = Q_SWTOF; break;
				case 8: op = Q_SLTOF; break;
				default: abort(); // Invariant
				}
			} else {
				assert(0); // TODO
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
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VOID:
		abort(); // Invariant
	}

	return result;
}

static void
gen_const_array_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	struct array_constant *aexpr = expr->constant.array;
	struct qbe_value base = mkqval(ctx, &out);

	size_t n = 0;
	const struct type *atype = type_dealias(expr->result);
	struct gen_value item = mktemp(ctx, atype->array.members, "item.%d");
	for (const struct array_constant *ac = aexpr; ac; ac = ac->next) {
		struct qbe_value offs = constl(n * atype->array.members->size);
		struct qbe_value ptr = mklval(ctx, &item);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, ac->value, item);
		++n;
	}

	if (!aexpr || !aexpr->expand) {
		return;
	}

	struct gen_value next = mktemp(ctx, atype->array.members, ".%d");
	struct qbe_value ptr = mklval(ctx, &next);

	size_t remain = atype->array.length - n;
	if (remain * atype->array.members->size <= 128) {
		struct gen_value last = gen_load(ctx, item);
		for (; n < atype->array.length; ++n) {
			struct qbe_value offs = constl(n * atype->array.members->size);
			pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
			gen_store(ctx, next, last);
		}
		return;
	}

	struct qbe_value offs = constl(n * atype->array.members->size);
	pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);

	struct qbe_value rtfunc = mkrtfunc(ctx, "rt.memcpy");
	struct qbe_value dtemp = mklval(ctx, &next);
	struct qbe_value stemp = mklval(ctx, &item);
	struct qbe_value sz = constl(remain * atype->array.members->size);
	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &dtemp, &stemp, &sz, NULL);
}

static void
gen_const_string_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	const struct expression_constant *constexpr = &expr->constant;
	const char *val = constexpr->string.value;
	size_t len = constexpr->string.len;

	// TODO: Generate string data structure as global also?
	struct qbe_value global = mkqtmp(ctx, ctx->arch.ptr, "strdata.%d");
	global.kind = QV_GLOBAL;

	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->name = global.name;
	def->kind = Q_DATA;
	def->data.items.type = QD_STRING;
	def->data.items.str = xcalloc(1, len);
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
gen_expr_const_at(struct gen_context *ctx,
	const struct expression *expr, struct gen_value out)
{
	if (!type_is_aggregate(type_dealias(expr->result))) {
		gen_store(ctx, out, gen_expr(ctx, expr));
		return;
	}

	switch (type_dealias(expr->result)->storage) {
	case STORAGE_ARRAY:
		gen_const_array_at(ctx, expr, out);
		break;
	case STORAGE_STRING:
		gen_const_string_at(ctx, expr, out);
		break;
	default:
		abort(); // Invariant
	}
}

static struct gen_value
gen_expr_const(struct gen_context *ctx, const struct expression *expr)
{
	if (type_is_aggregate(type_dealias(expr->result))) {
		struct gen_value out = mktemp(ctx, expr->result, "object.%d");
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
	switch (type_dealias(expr->result)->storage) {
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
		val.sval = (float)expr->constant.fval;
		return val;
	case Q_DOUBLE:
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
	defer->expr = expr->defer.deferred;
	defer->next = ctx->scope->defers;
	ctx->scope->defers = defer;
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

	push_scope(ctx);
	ctx->scope->label = expr->_for.label;
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
	const struct type *type = type_dealias(expr->alloc.expr->result);
	struct qbe_value rtfunc = mkrtfunc(ctx, "rt.free");
	struct gen_value val = gen_expr(ctx, expr->alloc.expr);
	struct qbe_value qval = mkqval(ctx, &val);
	if (type->storage == STORAGE_SLICE || type->storage == STORAGE_STRING) {
		struct qbe_value lval = mklval(ctx, &val);
		qval = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &qval, Q_LOADL, &lval, NULL);
	}
	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &qval, NULL);
	return gv_void;
}

static struct gen_value
gen_expr_if_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mktemp(ctx, expr->result, ".%d");
	}

	struct qbe_statement ltrue, lfalse, lend;
	struct qbe_value btrue = mklabel(ctx, &ltrue, "true.%d");
	struct qbe_value bfalse = mklabel(ctx, &lfalse, "false.%d");
	struct qbe_value bend = mklabel(ctx, &lend, ".%d");
	struct gen_value cond = gen_expr(ctx, expr->_if.cond);
	struct qbe_value qcond = mkqval(ctx, &cond);
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
gen_expr_list_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	push_scope(ctx);
	for (const struct expressions *exprs = &expr->list.exprs;
			exprs; exprs = exprs->next) {
		if (!exprs->next) {
			struct gen_value gv = gen_expr_with(
				ctx, exprs->expr, out);
			pop_scope(ctx, !exprs->expr->terminates);
			return gv;
		}
		gen_expr(ctx, exprs->expr);
	}
	abort(); // Unreachable
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

static void
gen_nested_match_tests(struct gen_context *ctx, struct gen_value object,
	struct qbe_value bmatch, struct qbe_value bnext,
	struct qbe_value tag, const struct match_case *_case,
	struct qbe_value *offset)
{
	// This function handles the case where we're matching against a type
	// which is a member of the tagged union, or an inner tagged union.
	//
	// type foo = (int | void);
	// type bar = (size | foo);
	//
	// let x: bar = 10i;
	// match (x) {
	// 	z: size => ...
	// 	i: int  => ...
	// 	void    => ...
	// };
	//
	// In the first case, we can simply test the object's tag. In the second
	// case, we have to test if the selected tag is 'foo', then check the
	// tag of the foo object for int.
	struct qbe_value *subtag = &tag;
	struct qbe_value subval = mkcopy(ctx, &object, "subval.%d");
	struct qbe_value match = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value temp = mkqtmp(ctx, &qbe_word, ".%d");
	const struct type *subtype = object.type;
	const struct type *test = _case->type;
	*offset = constl(subtype->align);
	do {
		struct qbe_statement lsubtype;
		struct qbe_value bsubtype = mklabel(ctx, &lsubtype, "subtype.%d");
		test = tagged_select_subtype(subtype, _case->type);
		if (!test) {
			abort(); // Invariant
		}

		struct qbe_value id = constw(test->id);
		pushi(ctx->current, &match, Q_CEQW, subtag, &id, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &match, &bsubtype, &bnext, NULL);
		push(&ctx->current->body, &lsubtype);

		if (test->id != _case->type->id) {
			struct qbe_value offs = constl(subtype->align);
			pushi(ctx->current, &subval, Q_ADD, &subval, &offs, NULL);
			pushi(ctx->current, &temp, Q_LOADUW, &subval, NULL);
			offset->lval += test->align;
			subtag = &temp;
		}

		subtype = test;
	} while (test->id != _case->type->id);

	pushi(ctx->current, NULL, Q_JMP, &bmatch, NULL);
}

static void
gen_subset_match_tests(struct gen_context *ctx,
	struct qbe_value bmatch, struct qbe_value bnext,
	struct qbe_value tag, const struct match_case *_case)
{
	// In this case, we're testing a case which is itself a tagged union,
	// and is a subset of the match object.
	//
	// type foo = (size | int | void);
	//
	// let x: foo = 10i;
	// match (x) {
	// 	n: (size | int) => ...
	// 	void => ...
	// };
	//
	// In this situation, we test the match object's tag against each type
	// ID of the case type.
	const struct type *casetype = type_dealias(_case->type);
	for (const struct type_tagged_union *tu = &casetype->tagged;
			tu; tu = tu->next) {
		struct qbe_statement lnexttag;
		struct qbe_value bnexttag = mklabel(ctx, &lnexttag, ".%d");
		struct qbe_value id = constl(tu->type->id);
		struct qbe_value match = mkqtmp(ctx, &qbe_word, ".%d");
		pushi(ctx->current, &match, Q_CEQW, &tag, &id, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &match, &bmatch, &bnexttag, NULL);
		push(&ctx->current->body, &lnexttag);
	}
	pushi(ctx->current, NULL, Q_JMP, &bnext, NULL);
}

static struct gen_value
gen_match_with_tagged(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	struct gen_value gvout = gv_void;
	if (!out) {
		gvout = mktemp(ctx, expr->result, ".%d");
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

		struct qbe_value offset;
		struct qbe_statement lmatch, lnext;
		struct qbe_value bmatch = mklabel(ctx, &lmatch, "matches.%d");
		struct qbe_value bnext = mklabel(ctx, &lnext, "next.%d");
		const struct type *subtype =
			tagged_select_subtype(objtype, _case->type);
		enum match_compat compat = COMPAT_SUBTYPE;
		if (subtype) {
			gen_nested_match_tests(ctx, object,
				bmatch, bnext, tag, _case, &offset);
		} else {
			assert(type_dealias(_case->type)->storage == STORAGE_TAGGED);
			assert(tagged_subset_compat(objtype, _case->type));
			if (tagged_align_compat(objtype, _case->type)) {
				compat = COMPAT_ALIGNED;
			} else {
				compat = COMPAT_MISALIGNED;
			}
			gen_subset_match_tests(ctx, bmatch, bnext, tag, _case);
		}

		push(&ctx->current->body, &lmatch);

		if (!_case->object) {
			goto next;
		}

		// TODO: We actually need to allocate a separate binding and
		// copy this into it, probably. We could avoid that if we knew
		// the binding were not assigned to, fwiw.
		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value.kind = GV_TEMP;
		gb->value.type = _case->type;
		gb->value.name = gen_name(ctx, "binding.%d");
		gb->object = _case->object;
		gb->next = ctx->bindings;
		ctx->bindings = gb;

		struct qbe_value qv = mklval(ctx, &gb->value);
		switch (compat) {
		case COMPAT_SUBTYPE:
			pushi(ctx->current, &qv, Q_ADD, &qobject, &offset, NULL);
			break;
		case COMPAT_ALIGNED:
			pushi(ctx->current, &qv, Q_COPY, &qobject, NULL);
			break;
		case COMPAT_MISALIGNED:
			;
			enum qbe_instr alloc = alloc_for_align(_case->type->align);
			struct qbe_value sz = constl(_case->type->size);
			pushprei(ctx->current, &qv, alloc, &sz, NULL);
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
		gvout = mktemp(ctx, expr->result, ".%d");
	}

	struct qbe_statement lout;
	struct qbe_value bout = mklabel(ctx, &lout, ".%d");
	struct gen_value object = gen_expr(ctx, expr->match.value);
	struct qbe_value qobject = mkqval(ctx, &object);

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

		if (_case->type->storage == STORAGE_NULL) {
			pushi(ctx->current, NULL, Q_JNZ,
				&qobject, &bnext, &bmatch, NULL);
		} else {
			pushi(ctx->current, NULL, Q_JNZ,
				&qobject, &bmatch, &bnext, NULL);
		}

		push(&ctx->current->body, &lmatch);

		if (!_case->object) {
			goto next;
		}

		struct gen_binding *gb = xcalloc(1, sizeof(struct gen_binding));
		gb->value = mktemp(ctx, _case->type, "binding.%d");
		gb->object = _case->object;
		gb->next = ctx->bindings;
		ctx->bindings = gb;

		// TODO: We could avoid this allocation if we knew the user
		// didn't mutate the binding.
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
	}

	push(&ctx->current->body, &lout);
	return gvout;
}

static struct gen_value
gen_expr_match_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	const struct type *objtype = expr->match.value->result;
	switch (type_dealias(objtype)->storage) {
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
		type = type_dereference(value->result);
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
			temp = mktemp(ctx, &builtin_type_size, ".%d");
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
	case M_SIZE:
		return (struct gen_value){
			.kind = GV_CONST,
			.type = &builtin_type_size,
			.lval = expr->measure.type->size,
		};
	case M_OFFSET:
		assert(0); // TODO
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
	if (type_dealias(ret.type)->storage == STORAGE_VOID) {
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
		struct qbe_value rtfunc = mkrtfunc(ctx, "rt.memset");
		struct qbe_value size =
			constl(expr->result->size), zero = constl(0);
		struct qbe_value base = mklval(ctx, &out);
		pushi(ctx->current, NULL, Q_CALL, &rtfunc,
			&base, &zero, &size, NULL);
	}

	struct gen_value ftemp = mktemp(ctx, &builtin_type_void, "field.%d");
	for (const struct expr_struct_field *field = &expr->_struct.fields;
			field; field = field->next) {
		if (!field->value) {
			assert(expr->_struct.autofill);
			field = field->next;
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
		gvout = mktemp(ctx, expr->result, ".%d");
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
	}

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
	const struct type *srctype = type_dealias(object.type);

	struct gen_value length;
	struct qbe_value qlength;
	struct qbe_value qbase;
	struct qbe_value qobject = mkqval(ctx, &object);
	struct qbe_value offset = constl(builtin_type_size.size);
	struct qbe_value qptr = mkqtmp(ctx, ctx->arch.ptr, ".%d");
	switch (srctype->storage) {
	case STORAGE_ARRAY:
		assert(srctype->array.length != SIZE_UNDEFINED);
		length = (struct gen_value){
			.kind = GV_CONST,
			.type = &builtin_type_size,
			.lval = srctype->array.length,
		};
		qlength = mkqval(ctx, &length);
		qbase = mkqval(ctx, &object);
		break;
	case STORAGE_SLICE:
		qbase = mkqtmp(ctx, ctx->arch.sz, "base.%d");
		enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
		pushi(ctx->current, &qbase, load, &qobject, NULL);
		length = mktemp(ctx, &builtin_type_size, "len.%d");
		qlength = mkqval(ctx, &length);
		pushi(ctx->current, &qptr, Q_ADD, &qobject, &offset, NULL);
		pushi(ctx->current, &qlength, load, &qptr, NULL);
		break;
	default: abort(); // Invariant
	}

	struct gen_value start;
	if (expr->slice.start) {
		start = gen_expr(ctx, expr->slice.start);
	} else {
		start = (struct gen_value){
			.kind = GV_CONST,
			.type = &builtin_type_size,
			.lval = 0,
		};
	}

	struct gen_value end;
	if (expr->slice.end) {
		end = gen_expr(ctx, expr->slice.end);
	} else {
		end = length;
	}

	struct qbe_value qstart = mkqval(ctx, &start);
	struct qbe_value qend = mkqval(ctx, &end);

	struct qbe_value start_oob = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value end_oob = mkqtmp(ctx, &qbe_word, ".%d");
	struct qbe_value valid = mkqtmp(ctx, &qbe_word, ".%d");
	pushi(ctx->current, &start_oob, Q_CULTL, &qstart, &qlength, NULL);
	pushi(ctx->current, &end_oob, Q_CULEL, &qend, &qlength, NULL);
	pushi(ctx->current, &valid, Q_AND, &start_oob, &end_oob, NULL);

	struct qbe_statement linvalid, lvalid;
	struct qbe_value binvalid = mklabel(ctx, &linvalid, ".%d");
	struct qbe_value bvalid = mklabel(ctx, &lvalid, ".%d");

	pushi(ctx->current, NULL, Q_JNZ, &valid, &bvalid, &binvalid, NULL);
	push(&ctx->current->body, &linvalid);
	gen_fixed_abort(ctx, expr->loc, ABORT_OOB);
	push(&ctx->current->body, &lvalid);

	struct qbe_value isz = constl(srctype->array.members->size);

	struct qbe_value qout = mkqval(ctx, &out);
	struct qbe_value data = mkqtmp(ctx, ctx->arch.ptr, "data.%d");
	pushi(ctx->current, &data, Q_MUL, &qstart, &isz, NULL);
	pushi(ctx->current, &data, Q_ADD, &qbase, &data, NULL);

	struct qbe_value newlen = mkqtmp(ctx, ctx->arch.sz, "newlen.%d");
	pushi(ctx->current, &newlen, Q_SUB, &qend, &qstart, NULL);
	struct qbe_value newcap = mkqtmp(ctx, ctx->arch.sz, "newcap.%d");
	pushi(ctx->current, &newcap, Q_SUB, &qlength, &qstart, NULL);

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

	const struct type *type = type_dealias(expr->result);
	struct gen_value vtemp = mktemp(ctx, &builtin_type_void, "value.%d");
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
		assert(operand->type == EXPR_ACCESS);
		val = gen_expr_access_addr(ctx, operand);
		val.type = expr->result;
		return val;
	case UN_DEREF:
		val = gen_expr(ctx, operand);
		assert(type_dealias(val.type)->storage == STORAGE_POINTER);
		val.type = type_dealias(val.type)->pointer.referent;
		return gen_load(ctx, val);
	case UN_BNOT:
		val = gen_expr(ctx, operand);
		temp = mktemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		struct qbe_value ones = constl((uint64_t)-1);
		pushi(ctx->current, &qtmp, Q_XOR, &qval, &ones, NULL);
		return temp;
	case UN_LNOT:
		val = gen_expr(ctx, operand);
		temp = mktemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		struct qbe_value one = constl(1);
		pushi(ctx->current, &qtmp, Q_XOR, &qval, &one, NULL);
		return temp;
	case UN_MINUS:
		val = gen_expr(ctx, operand);
		temp = mktemp(ctx, operand->result, ".%d");
		qval = mkqval(ctx, &val), qtmp = mkqval(ctx, &temp);
		struct qbe_value minusone = constl(-1);
		pushi(ctx->current, &qtmp, Q_MUL, &qval, &minusone, NULL);
		return temp;
	case UN_PLUS:
		return gen_expr(ctx, operand);
	}
	abort(); // Invariant
}

static struct gen_value
gen_expr(struct gen_context *ctx, const struct expression *expr)
{
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
		return gen_expr_control(ctx, expr);
	case EXPR_CALL:
		return gen_expr_call(ctx, expr);
	case EXPR_CAST:
		return gen_expr_cast(ctx, expr);
	case EXPR_CONSTANT:
		return gen_expr_const(ctx, expr);
	case EXPR_DEFER:
		return gen_expr_defer(ctx, expr);
	case EXPR_DELETE:
		assert(0); // TODO
	case EXPR_FOR:
		return gen_expr_for(ctx, expr);
	case EXPR_FREE:
		return gen_expr_free(ctx, expr);
	case EXPR_IF:
		return gen_expr_if_with(ctx, expr, NULL);
	case EXPR_INSERT:
		assert(0); // TODO
	case EXPR_LIST:
		return gen_expr_list_with(ctx, expr, NULL);
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
	case EXPR_SLICE:
	case EXPR_STRUCT:
	case EXPR_TUPLE:
		break; // Prefers -at style
	// gen-specific psuedo-expressions
	case EXPR_GEN_VALUE:
		return *(struct gen_value *)expr->user;
	}

	struct gen_value out = mktemp(ctx, expr->result, "object.%d");
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
	case EXPR_CONSTANT:
		gen_expr_const_at(ctx, expr, out);
		return;
	case EXPR_IF:
		gen_expr_if_with(ctx, expr, &out);
		return;
	case EXPR_LIST:
		gen_expr_list_with(ctx, expr, &out);
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

	if (func->flags & FN_TEST) {
		qdef->name = gen_name(ctx, "testfunc.%d");
	} else {
		qdef->name = decl->symbol ? strdup(decl->symbol)
			: ident_to_sym(&decl->ident);
	}

	struct qbe_statement start_label = {0};
	mklabel(ctx, &start_label, "start.%d");
	push(&qdef->func.prelude, &start_label);

	if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		qdef->func.returns = qtype_lookup(
			ctx, fntype->func.result, false);
	} else {
		qdef->func.returns = &qbe_void;
	}

	struct qbe_func_param *param, **next = &qdef->func.params;
	for (struct scope_object *obj = decl->func.scope->objects;
			obj; obj = obj->lnext) {
		const struct type *type = obj->type;
		param = *next = xcalloc(1, sizeof(struct qbe_func_param));
		assert(!obj->ident.ns); // Invariant
		param->name = strdup(obj->ident.name);
		param->type = qtype_lookup(ctx, type, false);

		struct gen_binding *gb =
			xcalloc(1, sizeof(struct gen_binding));
		gb->value.kind = GV_TEMP;
		gb->value.type = type;
		gb->object = obj;
		if (type_is_aggregate(type)) {
			// No need to copy to stack
			gb->value.name = strdup(param->name);
		} else {
			gb->value.name = gen_name(ctx, "param.%d");

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

	pushl(&qdef->func, &ctx->id, "body.%d");
	struct gen_value ret = gen_expr(ctx, decl->func.body);

	if (decl->func.body->terminates) {
		// XXX: This is a bit hacky, to appease qbe
		size_t ln = ctx->current->body.ln;
		struct qbe_statement *last = &ctx->current->body.stmts[ln - 1];
		if (last->type != Q_INSTR || last->instr != Q_RET) {
			pushi(ctx->current, NULL, Q_RET, NULL);
		}
	} else if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
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
				.name = strdup(qdef->name),
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
				.name = strdup(qdef->name),
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
		struct expression expr = {
			.type = EXPR_CONSTANT,
			.result = &builtin_type_str,
			.constant = {
				.object = NULL,
				.string = {
					.value = ident,
					.len = strlen(ident),
				},
			},
		};
		dataitem = gen_data_item(ctx, &expr, dataitem);

		struct qbe_data_item *next = xcalloc(1, sizeof *next);
		next->type = QD_VALUE;
		next->value.kind = QV_GLOBAL;
		next->value.type = &qbe_long;
		next->value.name = strdup(qdef->name);
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
	const struct type *type = type_dealias(expr->result);
	if (constant->object) {
		item->type = QD_SYMOFFS;
		item->sym = ident_to_sym(&constant->object->ident);
		item->offset = constant->ival;
		return item;
	}

	switch (type->storage) {
	case STORAGE_I8:
	case STORAGE_U8:
	case STORAGE_CHAR:
		item->type = QD_VALUE;
		item->value = constw((uint8_t)constant->uval);
		item->value.type = &qbe_byte;
		break;
	case STORAGE_I16:
	case STORAGE_U16:
		item->type = QD_VALUE;
		item->value = constw((uint16_t)constant->uval);
		item->value.type = &qbe_half;
		break;
	case STORAGE_BOOL:
		item->type = QD_VALUE;
		item->value = constw(constant->bval ? 1 : 0);
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
		assert(expr->type == EXPR_CONSTANT); // TODO?
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
		def->name = gen_name(ctx, "strdata.%d");
		def->kind = Q_DATA;
		def->data.items.type = QD_STRING;
		def->data.items.str = xcalloc(1, expr->constant.string.len);
		def->data.items.sz = expr->constant.string.len;
		memcpy(def->data.items.str, expr->constant.string.value,
			expr->constant.string.len);

		item->type = QD_VALUE;
		if (expr->constant.string.len != 0) {
			qbe_append_def(ctx->out, def);
			item->value.kind = QV_GLOBAL;
			item->value.type = &qbe_long;
			item->value.name = strdup(def->name);
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
		def->name = gen_name(ctx, "sldata.%d");
		def->kind = Q_DATA;

		size_t len = 0;
		struct qbe_data_item *subitem = &def->data.items;
		for (struct array_constant *c = constant->array;
				c; c = c->next) {
			gen_data_item(ctx, c->value, subitem);
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
			item->value.name = strdup(def->name);
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
			}
		}
		break;
	case STORAGE_ENUM:
		switch (type->_enum.storage) {
		case STORAGE_I8:
		case STORAGE_U8:
			item->type = QD_VALUE;
			item->value = constw((uint8_t)constant->uval);
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
			item->type = QD_VALUE;
			item->value = constw((uint32_t)constant->uval);
			break;
		case STORAGE_U64:
		case STORAGE_I64:
		case STORAGE_SIZE:
			item->type = QD_VALUE;
			item->value = constl((uint64_t)constant->uval);
			break;
		default:
			assert(0);
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
			}
		}
		break;
	case STORAGE_TAGGED:
	case STORAGE_UNION:
		assert(0); // TODO
	case STORAGE_ALIAS:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_NULL:
	case STORAGE_VOID:
		assert(0); // Invariant
	}

	assert(item->type != QD_VALUE || item->value.type);
	return item;
}

static void
gen_global_decl(struct gen_context *ctx, const struct declaration *decl)
{
	assert(decl->type == DECL_GLOBAL);
	const struct global_decl *global = &decl->global;
	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_DATA;
	qdef->exported = decl->exported;
	qdef->name = ident_to_sym(&decl->ident);
	gen_data_item(ctx, global->value, &qdef->data.items);
	qbe_append_def(ctx->out, qdef);
}

static void
gen_decl(struct gen_context *ctx, const struct declaration *decl)
{
	switch (decl->type) {
	case DECL_FUNC:
		gen_function_decl(ctx, decl);
		break;
	case DECL_GLOBAL:
		gen_global_decl(ctx, decl);
		break;
	case DECL_CONST:
	case DECL_TYPE:
		break; // Nothing to do
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
	const struct declarations *decls = unit->declarations;
	while (decls) {
		gen_decl(&ctx, decls->decl);
		decls = decls->next;
	}
}
