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

static void
gen_defers(struct gen_context *ctx)
{
	assert(ctx->scope);
	if (ctx->scope->defers) {
		pushc(ctx->current, "gen defers");
	}
	for (struct gen_defer *defer = ctx->scope->defers; defer;
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
		gen_defers(ctx);
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
	if (type_dealias(glval.type)->storage == STORAGE_SLICE) {
		enum qbe_instr load = load_for_type(ctx, &builtin_type_size);
		struct qbe_value temp = mkqtmp(ctx, ctx->arch.ptr, ".%d");
		pushi(ctx->current, &temp, load, &qlval, NULL);
		qlval = temp;
	}

	struct gen_value index = gen_expr(ctx, expr->access.index);
	struct qbe_value qindex = mkqval(ctx, &index);
	struct qbe_value itemsz = constl(expr->result->size);
	pushi(ctx->current, &qival, Q_MUL, &qindex, &itemsz, NULL);
	pushi(ctx->current, &qival, Q_ADD, &qlval, &qival, NULL);

	// TODO: Check bounds

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
gen_expr_assign(struct gen_context *ctx, const struct expression *expr)
{
	struct expression *object = expr->assign.object;
	struct expression *value = expr->assign.value;
	assert(object->type == EXPR_ACCESS || expr->assign.indirect); // Invariant
	assert(object->type != EXPR_SLICE); // TODO

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
		assert(0); // TODO
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

static struct gen_value
gen_expr_binarithm(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value lvalue = gen_expr(ctx, expr->binarithm.lvalue);
	struct gen_value rvalue = gen_expr(ctx, expr->binarithm.rvalue);
	struct gen_value result = mktemp(ctx, expr->result, ".%d");
	struct qbe_value qlval = mkqval(ctx, &lvalue);
	struct qbe_value qrval = mkqval(ctx, &rvalue);
	struct qbe_value qresult = mkqval(ctx, &result);
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
gen_expr_call(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value lvalue = gen_expr(ctx, expr->call.lvalue);
	lvalue = gen_autoderef(ctx, lvalue);

	const struct type *rtype = lvalue.type;
	assert(rtype->storage == STORAGE_FUNCTION);
	// TODO: Run deferred expressions if rtype->func.flags & FN_NORETURN

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

	struct gen_value value = gen_expr(ctx, expr->cast.value);
	struct qbe_value qvalue = mkqval(ctx, &value);
	struct qbe_value base = mklval(ctx, &out);
	struct qbe_value sz = constl(to->size);
	enum qbe_instr store = store_for_type(ctx, &builtin_type_size);
	pushi(ctx->current, NULL, store, &qvalue, &base, NULL);
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
			// Cast slice to pointer
			assert(0); // TODO
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
	assert(!aexpr->expand); // TODO
	struct qbe_value base = mkqval(ctx, &out);

	size_t index = 0;
	const struct type *atype = type_dealias(expr->result);
	struct gen_value item = mktemp(ctx, atype->array.members, "item.%d");
	for (const struct array_constant *ac = aexpr; ac; ac = ac->next) {
		struct qbe_value offs = constl(index * atype->array.members->size);
		struct qbe_value ptr = mklval(ctx, &item);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, ac->value, item);
		++index;
	}
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
	mklabel(ctx, &lafter, "after.%d");

	if (expr->_for.bindings) {
		gen_expr_binding(ctx, expr->_for.bindings);
	}

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

	pushi(ctx->current, NULL, Q_JMP, &bloop, NULL);

	push(&ctx->current->body, &lend);
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
	pushi(ctx->current, NULL, Q_JMP, &bend, NULL);

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
	struct qbe_value tag, struct match_case *_case,
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
	struct qbe_value tag, struct match_case *_case)
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
	struct match_case *_default = NULL;
	for (struct match_case *_case = expr->match.cases;
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
		gen_expr_with(ctx, _default->value, out);
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
	struct match_case *_default = NULL;
	for (struct match_case *_case = expr->match.cases;
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
		gen_expr_with(ctx, _default->value, out);
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
	const struct expression *value = expr->measure.value;
	switch (expr->measure.op) {
	case M_LEN:
		switch (type_dealias(value->result)->storage) {
		case STORAGE_ARRAY:
			len = type_dealias(value->result)->array.length;
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
	struct qbe_value qret = mkqval(ctx, &ret);
	gen_defers(ctx);
	pushi(ctx->current, NULL, Q_RET, &qret, NULL);
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
	case EXPR_APPEND:
		assert(0); // TODO
	case EXPR_ASSERT:
		return gen_expr_assert(ctx, expr);
	case EXPR_ASSIGN:
		return gen_expr_assign(ctx, expr);
	case EXPR_BINARITHM:
		return gen_expr_binarithm(ctx, expr);
	case EXPR_BINDING:
		return gen_expr_binding(ctx, expr);
	case EXPR_BREAK:
		assert(0); // TODO
	case EXPR_CALL:
		return gen_expr_call(ctx, expr);
	case EXPR_CAST:
		return gen_expr_cast(ctx, expr);
	case EXPR_CONSTANT:
		return gen_expr_const(ctx, expr);
	case EXPR_CONTINUE:
		assert(0); // TODO
	case EXPR_DEFER:
		return gen_expr_defer(ctx, expr);
	case EXPR_DELETE:
		assert(0); // TODO
	case EXPR_FOR:
		return gen_expr_for(ctx, expr);
	case EXPR_FREE:
		assert(0); // TODO
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
		assert(0); // TODO
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

static void
gen_function_decl(struct gen_context *ctx, const struct declaration *decl)
{
	const struct function_decl *func = &decl->func;
	const struct type *fntype = func->type;
	if (func->body == NULL) {
		return; // Prototype
	}
	// TODO: Attributes
	assert(!func->flags);

	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_FUNC;
	qdef->exported = decl->exported;
	qdef->name = decl->symbol ? strdup(decl->symbol)
		: ident_to_sym(&decl->ident);
	ctx->current = &qdef->func;

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
