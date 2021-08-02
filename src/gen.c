#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "scope.h"
#include "types.h"
#include "util.h"

static const struct gen_value gv_void = {
	.kind = GV_CONST,
	.type = &builtin_type_void,
};

static void
gen_copy_memcpy(struct gen_context *ctx,
	struct gen_value dest, struct gen_value src)
{
	struct qbe_value rtfunc = {
		.kind = QV_GLOBAL,
		.name = strdup("rt.memcpy"),
		.type = &qbe_long,
	};
	struct qbe_value sz = constl(dest.type->size);
	struct qbe_value dtemp = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = dest.name,
	}, stemp = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = src.name,
	};
	pushi(ctx->current, NULL, Q_CALL, &rtfunc,
			&dtemp, &stemp, &sz, NULL);
}

static void
gen_copy_struct(struct gen_context *ctx,
	struct gen_value dest, struct gen_value src)
{
	const struct type *stype = type_dealias(dest.type);
	assert(stype->storage == STORAGE_STRUCT);
	if (stype->size > 128) {
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
		.name = gen_name(ctx, "item.%d"),
	};
	struct qbe_value destp = mkcopy(ctx, &dest, "dest.%d");
	struct qbe_value srcp = mkcopy(ctx, &src, "src.%d");
	struct qbe_value align = constl(dest.type->align);
	for (size_t offset = 0; offset < dest.type->size;
			offset += dest.type->align) {
		pushi(ctx->current, &temp, load, &srcp, NULL);
		pushi(ctx->current, NULL, store, &temp, &destp, NULL);
		pushi(ctx->current, &srcp, Q_ADD, &srcp, &align, NULL);
		pushi(ctx->current, &destp, Q_ADD, &destp, &align, NULL);
	}
}

static void
gen_store(struct gen_context *ctx,
	struct gen_value object,
	struct gen_value value)
{
	switch (type_dealias(object.type)->storage) {
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
		assert(0); // TODO
	case STORAGE_STRUCT:
		gen_copy_struct(ctx, object, value);
		return;
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		assert(0); // TODO
	case STORAGE_UNION:
		gen_copy_memcpy(ctx, object, value);
		return;
	case STORAGE_ENUM:
		assert(0); // TODO
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
	switch (type_dealias(object.type)->storage) {
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
		assert(0); // TODO
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

static struct gen_value gen_expr(struct gen_context *ctx,
	const struct expression *expr);
static void gen_expr_at(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value out);
static struct gen_value gen_expr_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out);

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
gen_expr_access(struct gen_context *ctx, const struct expression *expr)
{
	struct gen_value addr;
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		addr = gen_access_ident(ctx, expr);
		break;
	case ACCESS_INDEX:
	case ACCESS_FIELD:
	case ACCESS_TUPLE:
		assert(0); // TODO
	}

	return gen_load(ctx, addr);
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
	assert(type_dealias(lvalue.type)->storage != STORAGE_POINTER); // TODO

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

static struct gen_value
gen_expr_const(struct gen_context *ctx, const struct expression *expr)
{
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
	case STORAGE_ARRAY:
		assert(0); // TODO
	case STORAGE_STRING:
		assert(0); // TODO
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
gen_expr_list_with(struct gen_context *ctx,
	const struct expression *expr,
	struct gen_value *out)
{
	// TODO: Set up defer scope
	for (const struct expressions *exprs = &expr->list.exprs;
			exprs; exprs = exprs->next) {
		if (!exprs->next) {
			return gen_expr_with(ctx, exprs->expr, out);
		}
		gen_expr(ctx, exprs->expr);
	}
	abort(); // Unreachable
}

static struct gen_value
gen_expr_return(struct gen_context *ctx, const struct expression *expr)
{
	// TODO: Run defers
	struct gen_value ret = gen_expr(ctx, expr->_return.value);
	struct qbe_value qret = mkqval(ctx, &ret);
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
		struct qbe_value rtfunc = {
			.kind = QV_GLOBAL,
			.name = strdup("rt.memset"),
			.type = &qbe_long,
		};
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
		struct qbe_value ptr = mkqval(ctx, &ftemp);
		ptr.type = ctx->arch.ptr;
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);
		gen_expr_at(ctx, field->value, ftemp);
	}
}

static struct gen_value
gen_expr(struct gen_context *ctx, const struct expression *expr)
{
	switch (expr->type) {
	case EXPR_ACCESS:
		return gen_expr_access(ctx, expr);
	case EXPR_ALLOC:
	case EXPR_APPEND:
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINARITHM:
		assert(0); // TODO
	case EXPR_BINDING:
		return gen_expr_binding(ctx, expr);
	case EXPR_BREAK:
		assert(0); // TODO
	case EXPR_CALL:
		return gen_expr_call(ctx, expr);
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		return gen_expr_const(ctx, expr);
	case EXPR_CONTINUE:
	case EXPR_DEFER:
	case EXPR_DELETE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
	case EXPR_INSERT:
		assert(0); // TODO
	case EXPR_LIST:
		return gen_expr_list_with(ctx, expr, NULL);
	case EXPR_MATCH:
	case EXPR_MEASURE:
		assert(0); // TODO
	case EXPR_PROPAGATE:
		assert(0); // Lowered in check (for now?)
	case EXPR_RETURN:
		return gen_expr_return(ctx, expr);
	case EXPR_SLICE:
	case EXPR_SWITCH:
	case EXPR_TUPLE:
	case EXPR_UNARITHM:
		assert(0); // TODO
	case EXPR_STRUCT:
		break; // Prefers -at style
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
	case EXPR_LIST:
		gen_expr_list_with(ctx, expr, &out);
		return;
	case EXPR_STRUCT:
		gen_expr_struct_at(ctx, expr, out);
		return;
	default:
		break; // Prefers non-at style
	}

	gen_store(ctx, out, gen_expr(ctx, expr));
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
	genl(&start_label, &ctx->id, "start.%d");
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
		// Do nothing
	} else if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		struct qbe_value qret = mkqval(ctx, &ret);
		pushi(ctx->current, NULL, Q_RET, &qret, NULL);
	} else {
		pushi(ctx->current, NULL, Q_RET, NULL);
	}

	qbe_append_def(ctx->out, qdef);
	ctx->current = NULL;
}

static void
gen_decl(struct gen_context *ctx, const struct declaration *decl)
{
	switch (decl->type) {
	case DECL_FUNC:
		gen_function_decl(ctx, decl);
		break;
	case DECL_GLOBAL:
		assert(0); // TODO
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
