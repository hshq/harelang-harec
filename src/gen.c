#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "identifier.h"
#include "qbe.h"
#include "scope.h"
#include "trace.h"
#include "types.h"
#include "util.h"

static void gen_expression(struct gen_context *ctx,
	const struct expression *expr, const struct qbe_value *out);

static struct gen_scope_context *
push_scope(struct gen_context *ctx,
	enum scope_class class,
	struct qbe_value *end)
{
	struct gen_scope_context *scope =
		xcalloc(1, sizeof(struct gen_scope_context));
	scope->class = class;
	scope->end = end;
	scope->parent = ctx->scope;
	scope->next_defer = &scope->defers;
	ctx->scope = scope;
	return scope;
}

static void
pop_scope(struct gen_context *ctx)
{
	struct gen_scope_context *scope = ctx->scope;
	assert(scope);
	struct gen_deferred *d = scope->defers;
	while (d) {
		struct gen_deferred *n = d->next;
		free(d);
		d = n;
	}
	ctx->scope = ctx->scope->parent;
	free(scope);
}

static void
gen_defers(struct gen_context *ctx, struct gen_scope_context *scope)
{
	struct gen_deferred *d = scope->defers;
	while (d) {
		gen_expression(ctx, d->expr, NULL);
		d = d->next;
	}
}

static char *
gen_name(struct gen_context *ctx, const char *fmt)
{
	int n = snprintf(NULL, 0, fmt, ctx->id);
	char *str = xcalloc(1, n + 1);
	snprintf(str, n + 1, fmt, ctx->id);
	++ctx->id;
	return str;
}

static void
gen_temp(struct gen_context *ctx, struct qbe_value *val,
		const struct qbe_type *type, const char *fmt)
{
	val->kind = QV_TEMPORARY;
	val->type = type;
	val->name = gen_name(ctx, fmt);
	val->indirect = false;
}

static void
alloc_temp(struct gen_context *ctx, struct qbe_value *val,
		const struct type *type, const char *fmt)
{
	struct qbe_value size = {0};
	gen_temp(ctx, val, qtype_for_type(ctx, type, true), fmt);
	val->indirect = true;
	constl(&size, type->size);
	const struct qbe_type *qtype = val->type;
	val->type = &qbe_long;
	pushprei(ctx->current, val, alloc_for_align(type->align), &size, NULL);
	val->type = qtype;
}

static void qval_for_object(struct gen_context *ctx,
	struct qbe_value *val, const struct scope_object *obj);

static struct gen_binding *
binding_alloc(struct gen_context *ctx, const struct scope_object *obj,
		struct qbe_value *val, const char *fmt)
{
	struct gen_binding *binding = xcalloc(1, sizeof(struct gen_binding));
	alloc_temp(ctx, val, obj->type, fmt);
	if (type_is_aggregate(obj->type)) {
		val->indirect = false;
	}
	binding->name = strdup(val->name);
	binding->object = obj;
	binding->next = ctx->bindings;
	ctx->bindings = binding;
	pushc(ctx->current, "alloc binding: %s -> %%%s, =%c (%s), indirect: %d",
			obj->ident.name, binding->name,
			(char)val->type->stype, val->type->name,
			val->indirect);
	return binding;
}

static const struct gen_binding *
binding_lookup(const struct gen_context *ctx, const struct scope_object *obj)
{
	struct gen_binding *binding = ctx->bindings;
	while (binding) {
		if (binding->object == obj) {
			return binding;
		}
		binding = binding->next;
	}
	return NULL;
}

static void
qval_for_object(struct gen_context *ctx,
	struct qbe_value *val,
	const struct scope_object *obj)
{
	const struct gen_binding *binding = NULL;
	switch (obj->otype) {
	case O_BIND:
		binding = binding_lookup(ctx, obj);
		val->kind = QV_TEMPORARY;
		val->indirect = true;
		val->name = strdup(binding->name);
		break;
	case O_DECL:
		val->kind = QV_GLOBAL;
		val->indirect = true;
		val->name = ident_to_sym(&obj->ident);
		break;
	case O_CONST:
	case O_TYPE:
		assert(0); // Invariant (lowered in check)
	}

	if (type_is_aggregate(obj->type)) {
		val->type = qtype_for_type(ctx, obj->type, true);
		val->indirect = false;
	} else {
		val->type = &qbe_long; // XXX: ARCH
	}
}

static void
qval_deref(struct qbe_value *val)
{
	val->indirect = val->type->stype != Q__AGGREGATE;
}

static void
qval_address(struct qbe_value *val)
{
	val->indirect = val->type->stype == Q__AGGREGATE;
}

static void
gen_copy(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src)
{
	assert(!dest->indirect && !src->indirect);
	pushc(ctx->current, "begin gen_copy for type %s (is_union? %d)",
			dest->type->name, dest->type->is_union);

	struct qbe_value temp = {0}, destp = {0}, srcp = {0}, size = {0};
	gen_temp(ctx, &temp, &qbe_long, "temp.%d");
	gen_temp(ctx, &destp, &qbe_long, "dest.%d");
	gen_temp(ctx, &srcp, &qbe_long, "src.%d");
	pushi(ctx->current, &destp, Q_COPY, dest, NULL);
	pushi(ctx->current, &srcp, Q_COPY, src, NULL);

	const struct qbe_field *field = &dest->type->fields;
	if (dest->type->is_union) {
		size_t max = 0;
		for (const struct qbe_field *f = &dest->type->fields;
				f; f = f->next) {
			if (f->type->size > max) {
				field = f;
			}
		}
	}

	size_t offset = 0;
	while (field) {
		temp.type = field->type;
		if (temp.type->stype == Q_HALF || temp.type->stype == Q_BYTE) {
			temp.type = &qbe_word;
		}

		for (size_t i = field->count; i > 0; --i) {
			struct qbe_value a, b;
			switch (field->type->stype) {
			case Q_BYTE:
			case Q_HALF:
			case Q_WORD:
			case Q_LONG:
			case Q_SINGLE:
			case Q_DOUBLE:
				pushi(ctx->current, &temp,
					load_for_type(field->type->stype,
						field->type->is_signed),
					&srcp, NULL);
				pushi(ctx->current, NULL,
					store_for_type(field->type->stype),
					&temp, &destp, NULL);
				break;
			case Q__AGGREGATE:
				a = destp, b = srcp;
				a.type = field->type;
				b.type = field->type;
				gen_copy(ctx, &a, &b);
				break;
			case Q__VOID:
				assert(0); // Invariant
			}

			if (!dest->type->is_union) {
				assert(field->type->size != 0);
				size_t add = field->type->size;
				offset += add;
				if (field->next && offset % field->next->type->size != 0) {
					add += offset % field->next->type->size;
					offset += offset % field->next->type->size;
				}
				constl(&size, add);
				pushi(ctx->current, &destp, Q_ADD, &destp, &size, NULL);
				pushi(ctx->current, &srcp, Q_ADD, &srcp, &size, NULL);
			}
		}

		field = field->next;

		if (dest->type->is_union) {
			// We only copy the largest field in this case
			break;
		}
	}

	pushc(ctx->current, "end gen_copy for type %s", dest->type->name);
}

static void
gen_store(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src)
{
	if (!dest) {
		// no-op
		return;
	}

	assert(src->type->stype != Q__VOID
		&& dest->type->stype != Q__VOID); // Invariant

	// TODO: Revisit me (again)
	if (src->type->stype == Q__AGGREGATE) {
		if (src->indirect && dest->indirect) {
			pushi(ctx->current, NULL, Q_STOREL, src, dest, NULL); // XXX: ARCH
		} else if (!dest->indirect && dest->type->stype == Q__AGGREGATE) {
			gen_copy(ctx, dest, src);
		} else {
			pushi(ctx->current, dest, Q_COPY, src, NULL);
		}
		return;
	}

	assert(!src->indirect);
	if (dest->indirect) {
		pushi(ctx->current, NULL,
			store_for_type(dest->type->stype),
			src, dest, NULL);
	} else {
		pushi(ctx->current, dest, Q_COPY, src, NULL);
	}
}

static void
gen_load(struct gen_context *ctx,
	const struct qbe_value *dest,
	const struct qbe_value *src,
	bool is_signed)
{
	assert(src->type->stype != Q__VOID
		&& dest->type->stype != Q__VOID); // Invariant

	if (src->type->stype == Q__AGGREGATE) {
		assert(!src->indirect);
		if (dest->type->stype == Q__AGGREGATE) {
			gen_copy(ctx, dest, src);
		} else {
			assert(dest->type == &qbe_long);
			pushi(ctx->current, dest, Q_COPY, src, NULL);
		}
		return;
	}

	assert(!dest->indirect);
	if (src->indirect) {
		pushi(ctx->current, dest,
			load_for_type(dest->type->stype, is_signed),
			src, NULL);
	} else {
		pushi(ctx->current, dest, Q_COPY, src, NULL);
	}
}

// Same as gen_load but dest is initialized to a new temporary
static void
gen_loadtemp(struct gen_context *ctx,
	struct qbe_value *dest, const struct qbe_value *src,
	const struct qbe_type *type, bool is_signed)
{
	gen_temp(ctx, dest, type, "load.%d");
	gen_load(ctx, dest, src, is_signed);
}

static void
address_ident(struct gen_context *ctx,
	const struct expression *expr,
	struct qbe_value *out)
{
	const struct scope_object *obj = expr->access.object;
	qval_for_object(ctx, out, obj);
}

static void
address_index(struct gen_context *ctx,
	const struct expression *expr,
	struct qbe_value *out)
{
	gen_temp(ctx, out, &qbe_long, "object.%d"); // XXX: ARCH
	gen_expression(ctx, expr->access.array, out);

	const struct type *atype = type_dealias(expr->access.array->result);
	if (atype->storage == TYPE_STORAGE_POINTER) {
		// We get one dereference for free for aggregate types
		atype = type_dealias(atype->pointer.referent);
	}
	while (atype->storage == TYPE_STORAGE_POINTER) {
		pushi(ctx->current, out, Q_LOADL, out, NULL);
		atype = type_dealias(atype->pointer.referent);
	}

	struct qbe_value index = {0};
	gen_temp(ctx, &index, &qbe_long, "index.%d");
	gen_expression(ctx, expr->access.index, &index);

	// XXX: ARCH
	struct qbe_value length = {0};
	if (atype->storage == TYPE_STORAGE_ARRAY) {
		constl(&length, atype->array.length);
	} else if (atype->storage == TYPE_STORAGE_SLICE) {
		struct qbe_value ptr = {0};
		gen_temp(ctx, &ptr, &qbe_long, "ptr.%d");
		constl(&length, 8);
		pushi(ctx->current, &ptr, Q_ADD, out, &length, NULL);
		gen_temp(ctx, &length, &qbe_long, "length.%d");
		pushi(ctx->current, &length, Q_LOADL, &ptr, NULL);
	}

	if (atype->storage != TYPE_STORAGE_ARRAY
			|| atype->array.length != SIZE_UNDEFINED) {
		struct qbe_statement validl = {0}, invalidl = {0};
		struct qbe_value bvalid = {0}, binvalid = {0};
		bvalid.kind = QV_LABEL;
		bvalid.name = strdup(genl(&validl, &ctx->id, "bounds.valid.%d"));
		binvalid.kind = QV_LABEL;
		binvalid.name = strdup(genl(&invalidl, &ctx->id, "bounds.invalid.%d"));

		struct qbe_value valid = {0};
		gen_temp(ctx, &valid, &qbe_word, "valid.%d");
		pushi(ctx->current, &valid, Q_CULTL, &index, &length, NULL);
		pushi(ctx->current, NULL, Q_JNZ, &valid, &bvalid, &binvalid, NULL);
		push(&ctx->current->body, &invalidl);

		struct qbe_value rtfunc = {0};
		rtfunc.kind = QV_GLOBAL;
		rtfunc.name = strdup("rt.abort_fixed");
		rtfunc.type = &qbe_long;
		constl(&valid, ABORT_OOB);
		pushi(ctx->current, NULL, Q_CALL, &rtfunc, &valid, NULL);

		push(&ctx->current->body, &validl);
	}

	if (atype->storage == TYPE_STORAGE_SLICE) {
		pushi(ctx->current, out, Q_LOADL, out, NULL);
	}

	struct qbe_value temp = {0};
	constl(&temp, atype->array.members->size);
	pushi(ctx->current, &index, Q_MUL, &index, &temp, NULL);
	pushi(ctx->current, out, Q_ADD, out, &index, NULL);
	if (!type_is_aggregate(atype->array.members)) {
		qval_deref(out);
	}
}

static void
address_field(struct gen_context *ctx,
	const struct expression *expr,
	struct qbe_value *out)
{
	gen_temp(ctx, out, &qbe_long, "object.%d"); // XXX: ARCH
	gen_expression(ctx, expr->access._struct, out);

	const struct type *stype = type_dealias(expr->access._struct->result);
	if (stype->storage == TYPE_STORAGE_POINTER) {
		// We get one dereference for free for aggregate types
		stype = type_dealias(stype->pointer.referent);
	}
	while (stype->storage == TYPE_STORAGE_POINTER) {
		pushi(ctx->current, out, Q_LOADL, out, NULL);
		stype = type_dealias(stype->pointer.referent);
	}

	const struct struct_field *field = expr->access.field;

	struct qbe_value offset = {0};
	constl(&offset, field->offset);
	pushi(ctx->current, out, Q_ADD, out, &offset, NULL);
	if (!type_is_aggregate(field->type)) {
		qval_deref(out);
	}
}

static void
address_object(struct gen_context *ctx,
		const struct expression *expr,
		struct qbe_value *out)
{
	assert(expr->type == EXPR_ACCESS);
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		address_ident(ctx, expr, out);
		break;
	case ACCESS_INDEX:
		address_index(ctx, expr, out);
		break;
	case ACCESS_FIELD:
		address_field(ctx, expr, out);
		break;
	}
}

static void
gen_expr_access(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (out == NULL) {
		pushc(ctx->current, "useless access expression discarded");
		return;
	}

	struct qbe_value src = {0}, temp = {0};
	address_object(ctx, expr, &src);
	if (src.indirect) {
		gen_loadtemp(ctx, &temp, &src,
			qtype_for_type(ctx, expr->result, false),
			type_is_signed(expr->result));
		gen_store(ctx, out, &temp);
	} else {
		gen_store(ctx, out, &src);
	}
}

static void
gen_slice_alloc(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value len = {0}, cap = {0}, size = {0}, temp = {0};
	gen_temp(ctx, &len,
		qtype_for_type(ctx, &builtin_type_size, true),
		"slice.len.%d");
	gen_temp(ctx, &cap,
		qtype_for_type(ctx, &builtin_type_size, true),
		"slice.cap.%d");
	gen_temp(ctx, &size,
		qtype_for_type(ctx, &builtin_type_size, true),
		"slice.size.%d");

	const struct expression *initializer = expr->alloc.expr;
	if (initializer->result->storage == TYPE_STORAGE_ARRAY) {
		assert(initializer->result->array.length != SIZE_UNDEFINED);
		constl(&len, initializer->result->array.length);
	} else {
		assert(0); // TODO: Initialize one slice from another
	}

	if (expr->alloc.cap != NULL) {
		gen_expression(ctx, expr->alloc.cap, &cap);
		constl(&temp, expr->result->array.members->size);
		pushi(ctx->current, &size, Q_MUL, &cap, &temp, NULL);
	} else {
		assert(0); // TODO: Alloc without explicit capacity
	}

	struct qbe_value ret = {0};
	gen_temp(ctx, &ret, &qbe_long, "alloc.ret.%d");
	struct qbe_value rtalloc = {0};
	rtalloc.kind = QV_GLOBAL;
	rtalloc.name = strdup("rt.malloc");
	rtalloc.type = &qbe_long;
	pushi(ctx->current, &ret, Q_CALL, &rtalloc, &size, NULL);

	// TODO: Generate abort if it failed

	assert(!out->indirect); // TODO?
	struct qbe_value ptr = {0};
	gen_temp(ctx, &ptr, &qbe_long, "alloc.ptr.%d");
	constl(&temp, 8); // XXX: ARCH
	pushi(ctx->current, &ptr, Q_COPY, out, NULL);
	pushi(ctx->current, NULL, Q_STOREL, &ret, &ptr, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &ptr, &temp, NULL);
	pushi(ctx->current, NULL, Q_STOREL, &len, &ptr, NULL);
	pushi(ctx->current, &ptr, Q_ADD, &ptr, &temp, NULL);
	pushi(ctx->current, NULL, Q_STOREL, &cap, &ptr, NULL);

	if (initializer->result->storage == TYPE_STORAGE_ARRAY) {
		gen_expression(ctx, initializer, &ret);
	} else {
		// TODO: I think this will have to be a separate branch once
		// we've implemented it
	}
}

static void
gen_alloc(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(expr->type == EXPR_ALLOC && expr->alloc.kind == AKIND_ALLOC);
	if (type_dealias(expr->result)->storage == TYPE_STORAGE_SLICE) {
		gen_slice_alloc(ctx, expr, out);
		return;
	}

	// TODO: Explicit capacity
	assert(expr->alloc.cap == NULL);
	struct qbe_value ret = {0}, size = {0};
	gen_temp(ctx, &size,
		qtype_for_type(ctx, &builtin_type_size, true),
		"alloc.size.%d");
	constl(&size, type_dealias(expr->result)->pointer.referent->size);
	// XXX: ARCH
	gen_temp(ctx, &ret, &qbe_long, "alloc.ret.%d");
	struct qbe_value rtalloc = {0};
	rtalloc.kind = QV_GLOBAL;
	rtalloc.name = strdup("rt.malloc");
	rtalloc.type = &qbe_long;
	pushi(ctx->current, &ret, Q_CALL, &rtalloc, &size, NULL);
	gen_store(ctx, out, &ret);
	qval_deref(&ret);
	struct qbe_value load = {0};
	gen_loadtemp(ctx, &load, out,
		qtype_for_type(ctx, expr->result, false),
		type_is_signed(expr->result));

	struct qbe_statement nulll = {0}, validl = {0}, endl = {0};
	struct qbe_value bnull = {0}, bvalid = {0}, bend = {0};
	bvalid.kind = QV_LABEL;
	bvalid.name = strdup(genl(&validl, &ctx->id, "alloc.valid.%d"));
	bnull.kind = QV_LABEL;
	bnull.name = strdup(genl(&nulll, &ctx->id, "alloc.null.%d"));
	bend.kind = QV_LABEL;
	bend.name = strdup(genl(&endl, &ctx->id, "alloc.end.%d"));
	// XXX: null might not be 0
	pushi(ctx->current, NULL, Q_JNZ, &load, &bvalid, &bnull, NULL);
	push(&ctx->current->body, &nulll);

	if (type_dealias(expr->result)->pointer.flags & PTR_NULLABLE) {
		pushi(ctx->current, NULL, Q_JMP, &bend, NULL);
	} else {
		struct qbe_value reason = {0}, rtabort = {0};
		constl(&reason, 2);
		rtabort.kind = QV_GLOBAL;
		rtabort.name = strdup("rt.abort_fixed");
		rtabort.type = &qbe_long;
		pushi(ctx->current, NULL, Q_CALL, &rtabort, &reason, NULL);
	}
	push(&ctx->current->body, &validl);
	if (!type_is_aggregate(type_dealias(expr->result)->pointer.referent)) {
		qval_deref(&load);
	}
	gen_expression(ctx, expr->alloc.expr, &load);
	push(&ctx->current->body, &endl);
}

static void
gen_expr_alloc(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(expr->type == EXPR_ALLOC);
	struct qbe_value val = {0}, rtfunc = {0};
	switch (expr->alloc.kind) {
	case AKIND_ALLOC:
		gen_alloc(ctx, expr, out);
		break;
	case AKIND_APPEND:
		assert(0); // TODO
	case AKIND_FREE:
		gen_temp(ctx, &val,
			qtype_for_type(ctx, expr->alloc.expr->result, true),
			"free.%d");
		if (type_dealias(expr->alloc.expr->result)->storage == TYPE_STORAGE_SLICE) {
			qval_address(&val);
			gen_expression(ctx, expr->alloc.expr, &val);
			val.type = &qbe_long;
			pushi(ctx->current, &val, Q_LOADL, &val, NULL);
		} else {
			gen_expression(ctx, expr->alloc.expr, &val);
		}
		rtfunc.kind = QV_GLOBAL;
		rtfunc.name = strdup("rt.free");
		rtfunc.type = &qbe_long;
		pushi(ctx->current, NULL, Q_CALL, &rtfunc, &val, NULL);
		break;
	}
}

static void
gen_expr_assert(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(expr->assert.message); // Invariant

	struct qbe_statement failedl = {0}, passedl = {0};
	struct qbe_value bfailed = {0}, bpassed = {0};

	struct qbe_value msg = {0};

	struct qbe_value rtfunc = {0};
	rtfunc.kind = QV_GLOBAL;
	rtfunc.name = strdup("rt.abort");
	rtfunc.type = &qbe_long;

	if (expr->assert.cond) {
		bfailed.kind = QV_LABEL;
		bfailed.name = strdup(genl(&failedl, &ctx->id, "failed.%d"));
		bpassed.kind = QV_LABEL;
		bpassed.name = strdup(genl(&passedl, &ctx->id, "passed.%d"));

		struct qbe_value cond = {0};
		gen_temp(ctx, &cond, &qbe_word, "assert.%d");
		gen_expression(ctx, expr->assert.cond, &cond);

		alloc_temp(ctx, &msg, &builtin_type_const_str, "msg.%d");
		qval_deref(&msg);
		gen_expression(ctx, expr->assert.message, &msg);

		pushi(ctx->current, NULL, Q_JNZ, &cond, &bpassed, &bfailed, NULL);
		push(&ctx->current->body, &failedl);
	} else {
		alloc_temp(ctx, &msg, &builtin_type_const_str, "msg.%d");
		qval_deref(&msg);
		gen_expression(ctx, expr->assert.message, &msg);
	}

	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &msg, NULL);

	if (expr->assert.cond) {
		push(&ctx->current->body, &passedl);
	}
}

static void
gen_expr_assign(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL); // Invariant

	struct expression *object = expr->assign.object;
	assert(object->type == EXPR_ACCESS || expr->assign.indirect); // Invariant

	const struct expression *value = expr->assign.value;
	const struct type *objtype = expr->assign.indirect
		? object->result->pointer.referent : object->result;
	const struct qbe_type *vtype =
		qtype_for_type(ctx, value->result, true);
	const struct qbe_type *otype = qtype_for_type(ctx, objtype, true);

	struct qbe_value src = {0};
	if (expr->assign.indirect) {
		gen_temp(ctx, &src, &qbe_long, "indirect.%d"); // XXX: ARCH
		gen_expression(ctx, object, &src);
		src.type = otype;
		qval_deref(&src);
	} else {
		const struct expression *object = expr->assign.object;
		address_object(ctx, object, &src);
		src.type = qtype_for_type(ctx, object->result, true);
	}

	if (expr->assign.op == BIN_LEQUAL) {
		if (!type_is_aggregate(value->result)) {
			qval_deref(&src);
		}
		gen_expression(ctx, value, &src);
	} else {
		struct qbe_value v = {0};
		gen_temp(ctx, &v, vtype, "assign.value.%d");
		gen_expression(ctx, value, &v);

		struct qbe_value result;
		gen_temp(ctx, &result, otype, "assign.result.%d");

		struct qbe_value load;
		gen_loadtemp(ctx, &load, &src, otype,
			type_is_signed(objtype));
		pushi(ctx->current, &result,
			binarithm_for_op(expr->assign.op, otype,
				type_is_signed(objtype)),
			&load, &v, NULL);
		gen_store(ctx, &src, &result);
	}
}

static void gen_global_decl(struct gen_context *ctx,
		const struct declaration *decl);

static void
gen_expr_binding(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL);

	const struct expression_binding *binding = &expr->binding;
	while (binding) {
		struct qbe_value temp = {0};
		if (binding->object->otype != O_DECL) {
			binding_alloc(ctx, binding->object, &temp, "binding.%d");
			gen_expression(ctx, binding->initializer, &temp);
		} else {
			struct declaration decl = {
				.type = DECL_GLOBAL,
				.ident = binding->object->ident,
				.global = {
					.type = binding->object->type,
					.value = binding->initializer,
				},
			};
			gen_global_decl(ctx, &decl);
		}
		binding = binding->next;
	}
}

static void
extend(struct gen_context *ctx, struct qbe_value *v, const struct type *type)
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
		return;
	}

	struct qbe_value temp = {0};
	gen_temp(ctx, &temp, &qbe_word, "ext.%d");
	pushi(ctx->current, &temp, op, v, NULL);
	*v = temp;
}

static void
gen_expr_binarithm(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct qbe_type *ltype =
		qtype_for_type(ctx, expr->binarithm.lvalue->result, false);
	const struct qbe_type *rtype =
		qtype_for_type(ctx, expr->binarithm.rvalue->result, false);
	const struct qbe_type *etype = qtype_for_type(ctx, expr->result, false);
	assert(ltype == rtype); // TODO: Type promotion

	if (expr->binarithm.op == BIN_LAND || expr->binarithm.op == BIN_LOR) {
		struct qbe_statement rlabel = {0}, slabel = {0};
		struct qbe_value rbranch = {0}, sbranch = {0}, result = {0};
		rbranch.kind = QV_LABEL;
		rbranch.name = strdup(genl(&rlabel, &ctx->id, "rvalue.%d"));
		sbranch.kind = QV_LABEL;
		sbranch.name = strdup(genl(&slabel, &ctx->id, "short_circuit.%d"));
		gen_temp(ctx, &result, etype, "result.%d");

		gen_expression(ctx, expr->binarithm.lvalue, &result);
		if (expr->binarithm.op == BIN_LAND) {
			pushi(ctx->current, NULL, Q_JNZ, &result, &rbranch,
				&sbranch, NULL);
		} else {
			pushi(ctx->current, NULL, Q_JNZ, &result, &sbranch,
				&rbranch, NULL);
		}

		push(&ctx->current->body, &rlabel);
		gen_expression(ctx, expr->binarithm.rvalue, &result);
		if (!expr->binarithm.rvalue->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &sbranch, NULL);
		}

		push(&ctx->current->body, &slabel);
		gen_store(ctx, out, &result);
		return;
	}

	struct qbe_value lvalue = {0}, rvalue = {0}, result = {0};
	gen_temp(ctx, &lvalue, ltype, "lvalue.%d");
	gen_temp(ctx, &rvalue, rtype, "rvalue.%d");
	gen_temp(ctx, &result, etype, "result.%d");

	gen_expression(ctx, expr->binarithm.lvalue, &lvalue);
	gen_expression(ctx, expr->binarithm.rvalue, &rvalue);

	switch (expr->binarithm.op) {
	case BIN_GREATER:
	case BIN_GREATEREQ:
	case BIN_LEQUAL:
	case BIN_LESS:
	case BIN_LESSEQ:
	case BIN_NEQUAL:
		extend(ctx, &lvalue, expr->binarithm.lvalue->result);
		extend(ctx, &rvalue, expr->binarithm.rvalue->result);
		break;
	default:
		break;
	}

	pushi(ctx->current, &result,
		binarithm_for_op(expr->binarithm.op, ltype,
			type_is_signed(expr->binarithm.lvalue->result)),
		&lvalue, &rvalue, NULL);
	gen_store(ctx, out, &result);
}

static void
gen_expr_call(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value result = {0};

	struct qbe_statement call = {
		.type = Q_INSTR,
		.instr = Q_CALL,
	};

	struct qbe_arguments *arg, **next = &call.args;
	struct call_argument *carg = expr->call.args;
	arg = *next = xcalloc(1, sizeof(struct qbe_arguments));
	gen_temp(ctx, &arg->value, &qbe_long, "func.%d");
	gen_expression(ctx, expr->call.lvalue, &arg->value);
	next = &arg->next;

	const struct type *ftype = type_dealias(expr->call.lvalue->result);
	if (ftype->storage == TYPE_STORAGE_POINTER) {
		// We get one dereference for free for aggregate types
		ftype = type_dealias(ftype->pointer.referent);
	}
	while (ftype->storage == TYPE_STORAGE_POINTER) {
		pushi(ctx->current, &arg->value, Q_LOADL, &arg->value, NULL);
		ftype = type_dealias(ftype->pointer.referent);
	}
	assert(ftype->storage == TYPE_STORAGE_FUNCTION);
	if (ftype->func.result != &builtin_type_void) {
		gen_temp(ctx, &result, qtype_for_type(ctx,
			ftype->func.result, true), "returns.%d");
		call.out = qval_dup(&result);
	}

	while (carg) {
		arg = *next = xcalloc(1, sizeof(struct qbe_arguments));
		if (type_is_aggregate(carg->value->result)) {
			alloc_temp(ctx, &arg->value,
				carg->value->result, "arg.%d");
			qval_deref(&arg->value);
		} else {
			gen_temp(ctx, &arg->value,
				qtype_for_type(ctx, carg->value->result, false),
				"arg.%d");
		}
		gen_expression(ctx, carg->value, &arg->value);
		carg = carg->next;
		next = &arg->next;
	}

	push(&ctx->current->body, &call);

	if (out) {
		gen_store(ctx, out, &result);
	}
}

static void
gen_cast_to_tagged(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out,
	const struct type *from)
{
	const struct type *tagged = expr->result;
	const struct type *subtype = tagged_select_subtype(tagged, from);

	struct qbe_value tag = {0}, ptr = {0}, offs = {0};
	gen_temp(ctx, &ptr, &qbe_long, "to_tagged.from.%d");
	constl(&offs, expr->result->align);

	if (!subtype) {
		pushc(ctx->current, "to_tagged; no subtype");
		gen_expression(ctx, expr->cast.value, &ptr);
		gen_copy(ctx, out, &ptr);
		return;
	}

	pushc(ctx->current, "to_tagged; valid subtype");
	// TODO: check should lower this to multiple casts:
	assert(subtype->id == from->id);

	constw(&tag, subtype->id);
	pushi(ctx->current, &ptr, Q_COPY, out, NULL);
	pushi(ctx->current, NULL, Q_STOREW, &tag, &ptr, NULL);

	struct qbe_value *storage;
	if (expr->cast.value->result->size == 0) {
		storage = NULL;
	} else {
		pushi(ctx->current, &ptr, Q_ADD, &ptr, &offs, NULL);
		ptr.type = qtype_for_type(ctx, expr->cast.value->result, false);
		ptr.indirect = !type_is_aggregate(expr->cast.value->result);
		storage = &ptr;
	}

	gen_expression(ctx, expr->cast.value, storage);
}

static void
gen_cast_from_tagged(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out,
	const struct type *to)
{
	if (type_dealias(to)->storage == TYPE_STORAGE_VOID) {
		return;
	}

	if (type_dealias(to)->storage == TYPE_STORAGE_TAGGED) {
		assert(0); // TODO
	}

	const struct type *tagged = expr->cast.value->result;
	struct qbe_value object = {0}, offs = {0}, temp = {0};
	alloc_temp(ctx, &object, tagged, "from.tagged.%d");
	gen_expression(ctx, expr->cast.value, &object);

	struct qbe_value ptr = {0};
	gen_temp(ctx, &ptr, &qbe_long, "ptr.%d");
	constl(&offs, expr->cast.value->result->align);
	pushi(ctx->current, &ptr, Q_ADD, &object, &offs, NULL);

	ptr.type = qtype_for_type(ctx, expr->result, false);
	qval_deref(&ptr);
	if (object.indirect) {
		gen_loadtemp(ctx, &temp, &ptr,
			qtype_for_type(ctx, expr->result, false),
			type_is_signed(expr->result));
		gen_store(ctx, out, &temp);
	} else {
		gen_store(ctx, out, &ptr);
	}
}

static void
gen_expr_type_test(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	// XXX: ARCH
	const struct type *want = expr->cast.secondary,
	      *tagged = type_dealias(expr->cast.value->result);
	struct qbe_value tag = {0}, in = {0}, id = {0};
	gen_temp(ctx, &tag, &qbe_word, "tag.%d");
	gen_temp(ctx, &in, qtype_for_type(ctx, tagged, false), "cast.in.%d");
	qval_address(&in);
	gen_expression(ctx, expr->cast.value, &in);
	pushi(ctx->current, &tag, Q_LOADUW, &in, NULL);
	constl(&id, want->id);
	pushi(ctx->current, out, Q_CEQW, &tag, &id, NULL);
}

static void
gen_expr_type_assertion(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	// XXX: ARCH
	const struct type *want = type_dealias(expr->cast.secondary),
	      *tagged = type_dealias(expr->cast.value->result);
	struct qbe_value tag = {0}, in = {0}, id = {0}, result = {0};
	gen_temp(ctx, &tag, &qbe_word, "tag.%d");
	gen_temp(ctx, &in, qtype_for_type(ctx, tagged, false), "cast.in.%d");
	qval_address(&in);
	gen_expression(ctx, expr->cast.value, &in);
	pushi(ctx->current, &tag, Q_LOADUW, &in, NULL);
	constw(&id, want->id);
	gen_temp(ctx, &result, &qbe_word, "valid.%d");
	pushi(ctx->current, &result, Q_CEQW, &tag, &id, NULL);

	struct qbe_statement validl = {0}, invalidl = {0};
	struct qbe_value bvalid = {0}, binvalid = {0};
	bvalid.kind = QV_LABEL;
	bvalid.name = strdup(genl(&validl, &ctx->id, "type.valid.%d"));
	binvalid.kind = QV_LABEL;
	binvalid.name = strdup(genl(&invalidl, &ctx->id, "type.invalid.%d"));

	pushi(ctx->current, NULL, Q_JNZ, &result, &bvalid, &binvalid, NULL);

	push(&ctx->current->body, &invalidl);

	struct qbe_value rtfunc = {0};
	rtfunc.kind = QV_GLOBAL;
	rtfunc.name = strdup("rt.abort_fixed");
	rtfunc.type = &qbe_long;
	constl(&result, ABORT_TYPE_ASSERTION);
	pushi(ctx->current, NULL, Q_CALL, &rtfunc, &result, NULL);

	push(&ctx->current->body, &validl);
}

static void
gen_expr_cast(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	switch (expr->cast.kind) {
	case C_CAST:
		break; // Handled below
	case C_ASSERTION:
		gen_expr_type_assertion(ctx, expr, out);
		break; // Handled below
	case C_TEST:
		gen_expr_type_test(ctx, expr, out);
		return;
	}

	const struct type *to = expr->result, *from = expr->cast.value->result;
	if (type_dealias(to)->storage == TYPE_STORAGE_TAGGED) {
		gen_cast_to_tagged(ctx, expr, out, from);
		return;
	} else if (type_dealias(from)->storage == TYPE_STORAGE_TAGGED) {
		gen_cast_from_tagged(ctx, expr, out, to);
		return;
	}

	to = type_dealias(to), from = type_dealias(from);
	if (to->storage == from->storage && to->size == from->size) {
		gen_expression(ctx, expr->cast.value, out);
		return;
	}

	bool is_signed = type_is_signed(from);

	struct qbe_value in = {0}, result = {0};
	gen_temp(ctx, &result, qtype_for_type(ctx, to, false), "cast.out.%d");

	// Special case: str -> *const char
	if (to->storage == TYPE_STORAGE_POINTER
			&& to->pointer.referent->storage == TYPE_STORAGE_CHAR
			&& from->storage == TYPE_STORAGE_STRING) {
		alloc_temp(ctx, &in, from, "cast.in.%d");
		in.indirect = false;
		gen_expression(ctx, expr->cast.value, &in);
		in.type = &qbe_long;
		qval_deref(&in);
		gen_load(ctx, &result, &in, false);
		gen_store(ctx, out, &result);
		return;
	}

	if (to->storage == TYPE_STORAGE_VOID) {
		gen_expression(ctx, expr->cast.value, NULL);
		return;
	}

	if (type_is_aggregate(expr->cast.value->result)
			&& expr->cast.value->type == EXPR_CONSTANT) {
		// This is a stupid fucking hack
		alloc_temp(ctx, &in, expr->cast.value->result, "cast.in.%d");
		qval_deref(&in);
	} else {
		gen_temp(ctx, &in, qtype_for_type(ctx, from, false), "cast.in.%d");
	}
	gen_expression(ctx, expr->cast.value, &in);

	// Used for various casts
	enum qbe_instr op;
	struct qbe_value ptr = {0}, offs = {0}, len = {0};

	switch (to->storage) {
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_INT:		// XXX: ARCH
	case TYPE_STORAGE_UINT:		// XXX: ARCH
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINTPTR:	// XXX: ARCH
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:		// XXX: ARCH
		if (type_is_integer(from) && to->size <= from->size) {
			op = Q_COPY;
		} else if (type_is_integer(from) && to->size > from->size) {
			switch (from->size) {
			case 4:
				op = is_signed ? Q_EXTSW : Q_EXTUW;
				break;
			case 2:
				op = is_signed ? Q_EXTSH : Q_EXTUH;
				break;
			case 1:
				op = is_signed ? Q_EXTSB : Q_EXTUB;
				break;
			default:
				assert(0); // Invariant
			}
		} else if (from->storage == TYPE_STORAGE_POINTER
				|| from->storage == TYPE_STORAGE_NULL) {
			assert(to->storage == TYPE_STORAGE_UINTPTR);
			op = Q_COPY;
		} else if (from->storage == TYPE_STORAGE_RUNE) {
			assert(to->storage == TYPE_STORAGE_U32);
			op = Q_COPY;
		} else {
			assert(0); // Invariant
		}
		pushi(ctx->current, &result, op, &in, NULL);
		break;
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		assert(0); // TODO
	case TYPE_STORAGE_ARRAY:
		if (from->storage == TYPE_STORAGE_ARRAY) {
			pushi(ctx->current, &result, Q_COPY, &in, NULL);
			break;
		}
		assert(0); // TODO: Convert slice to array
		break;
	case TYPE_STORAGE_SLICE:
		if (from->storage == TYPE_STORAGE_SLICE) {
			pushi(ctx->current, &result, Q_COPY, &in, NULL);
			break;
		}
		// XXX: ARCH
		gen_temp(ctx, &ptr, &qbe_long, "ptr.%d");
		constl(&offs, 8);
		constl(&len, from->array.length);
		pushi(ctx->current, &ptr, Q_COPY, out, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &in, &ptr, NULL);
		pushi(ctx->current, &ptr, Q_ADD, &ptr, &offs, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &len, &ptr, NULL);
		pushi(ctx->current, &ptr, Q_ADD, &ptr, &offs, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &len, &ptr, NULL);
		return;
	// Can be implemented with a copy
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_POINTER:
		pushi(ctx->current, &result, Q_COPY, &in, NULL);
		break;
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_TAGGED:
		assert(0); // Handled above
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		assert(0); // Invariant
	case TYPE_STORAGE_VOID:
		return; // no-op
	}

	gen_store(ctx, out, &result);
}

static void
gen_array(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct type *type = expr->result;
	assert(type->array.length != SIZE_UNDEFINED);
	if (type->array.length == 0) {
		return;
	}

	// XXX: ARCH
	struct qbe_value ptr = {0}, val = {0};
	gen_temp(ctx, &val,
		qtype_for_type(ctx, type->array.members, true),
		"ptr.%d");
	ptr = val, ptr.type = &qbe_long;
	qval_deref(&val);
	pushi(ctx->current, &ptr, Q_COPY, out, NULL);

	struct qbe_value size = {0};
	constl(&size, type->array.members->size);

	size_t n = 0;
	struct array_constant *item = expr->constant.array;
	while (item) {
		gen_expression(ctx, item->value, &val);
		++n;
		if (item->next) {
			pushi(ctx->current, &ptr, Q_ADD, &ptr, &size, NULL);
		}
		item = item->next;
	}

	if (!expr->constant.array->expand) {
		return;
	}

	pushc(ctx->current, "expanding array to length %zd", type->array.length);
	struct qbe_value last = {0};
	if (type_is_aggregate(type->array.members)) {
		alloc_temp(ctx, &last, type->array.members, "expand.%d");
		qval_deref(&last);
	} else {
		gen_temp(ctx, &last,
			qtype_for_type(ctx, type->array.members, false),
			"expand.%d");
	}
	gen_load(ctx, &last, &val, type_is_signed(type->array.members));
	for (; n < type->array.length; ++n) {
		pushi(ctx->current, &ptr, Q_ADD, &ptr, &size, NULL);
		gen_store(ctx, &val, &last);
	}
}

static void
gen_string(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(!out->indirect); // Invariant

	struct qbe_value temp = {0};
	gen_temp(ctx, &temp, &qbe_long, "strdata.%d");
	temp.kind = QV_GLOBAL;

	struct qbe_def *def = xcalloc(1, sizeof(struct qbe_def));
	def->name = temp.name;
	def->kind = Q_DATA;
	def->data.items.type = QD_STRING;
	def->data.items.str = xcalloc(1, expr->constant.string.len);
	memcpy(def->data.items.str, expr->constant.string.value,
			expr->constant.string.len);
	def->data.items.sz = expr->constant.string.len;

	struct qbe_value size = {0};
	constl(&size, expr->constant.string.len); // XXX: ARCH

	struct qbe_value str = {0};
	gen_temp(ctx, &str, &qbe_long, "str.%d"); // XXX: ARCH
	pushi(ctx->current, &str, Q_COPY, out, NULL);
	str.indirect = true;

	if (expr->constant.string.len != 0) {
		qbe_append_def(ctx->out, def);
	} else {
		free(def);
		constl(&temp, 0);
	}

	gen_store(ctx, &str, &temp);
	constl(&temp, 8); // XXX: ARCH
	pushi(ctx->current, &str, Q_ADD, &str, &temp, NULL);
	gen_store(ctx, &str, &size);
	pushi(ctx->current, &str, Q_ADD, &str, &temp, NULL);
	gen_store(ctx, &str, &size);
}

static void
gen_expr_constant(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (out == NULL) {
		pushc(ctx->current, "useless constant expression discarded");
		return;
	}

	struct qbe_value val = {0};

	// Special cases
	switch (expr->result->storage) {
	case TYPE_STORAGE_BOOL:
		constw(&val, expr->constant.bval ? 1 : 0);
		gen_store(ctx, out, &val);
		return;
	case TYPE_STORAGE_VOID:
		const_void(&val);
		gen_store(ctx, out, &val);
		return;
	case TYPE_STORAGE_NULL:
		constl(&val, 0);
		gen_store(ctx, out, &val);
		return;
	case TYPE_STORAGE_ARRAY:
		gen_array(ctx, expr, out);
		return;
	case TYPE_STORAGE_STRING:
		gen_string(ctx, expr, out);
		return;
	default:
		// Moving right along
		break;
	}

	const struct qbe_type *qtype = qtype_for_type(ctx, expr->result, false);
	switch (qtype->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
		constw(&val, (uint32_t)expr->constant.uval);
		break;
	case Q_LONG:
		constl(&val, (uint64_t)expr->constant.uval);
		break;
	case Q_SINGLE:
		consts(&val, (float)expr->constant.fval);
		break;
	case Q_DOUBLE:
		constd(&val, expr->constant.fval);
		break;
	case Q__VOID:
		return; // no-op
	case Q__AGGREGATE:
		assert(0); // Invariant
	}

	gen_store(ctx, out, &val);
}

static void
gen_expr_control(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL); // Invariant
	struct gen_scope_context *scope = ctx->scope;
	while (scope != NULL) {
		gen_defers(ctx, scope);
		if (expr->control.label && scope->label) {
			if (strcmp(expr->control.label, scope->label) == 0) {
				break;
			}
		} else if (scope->class == SCOPE_LOOP) {
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
}

static void
gen_expr_defer(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct gen_deferred *d = xcalloc(1, sizeof(struct gen_deferred));
	d->expr = expr->defer.deferred;
	*ctx->scope->next_defer = d;
	ctx->scope->next_defer = &d->next;
}

static void
gen_expr_for(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	assert(out == NULL); // Invariant
	if (expr->_for.bindings) {
		gen_expr_binding(ctx, expr->_for.bindings, NULL);
	}

	struct qbe_statement loopl = {0}, bodyl = {0}, afterl = {0}, endl = {0};
	struct qbe_value loop = {0}, body = {0}, after = {0}, end = {0};
	loop.kind = QV_LABEL;
	loop.name = strdup(genl(&loopl, &ctx->id, "loop.%d"));
	body.kind = QV_LABEL;
	body.name = strdup(genl(&bodyl, &ctx->id, "body.%d"));
	after.kind = QV_LABEL;
	after.name = strdup(genl(&afterl, &ctx->id, "after.%d"));
	end.kind = QV_LABEL;
	end.name = strdup(genl(&endl, &ctx->id, "end.%d"));

	push(&ctx->current->body, &loopl);

	struct gen_scope_context *scope = push_scope(ctx, SCOPE_LOOP, &end);
	scope->after = &after;
	scope->label = expr->_for.label;

	struct qbe_value cond = {0};
	gen_temp(ctx, &cond, &qbe_word, "cond.%d");
	gen_expression(ctx, expr->_for.cond, &cond);

	pushi(ctx->current, NULL, Q_JNZ, &cond, &body, &end, NULL);

	push(&ctx->current->body, &bodyl);
	gen_expression(ctx, expr->_for.body, NULL);

	push(&ctx->current->body, &afterl);
	if (expr->_for.afterthought) {
		gen_expression(ctx, expr->_for.afterthought, NULL);
	}

	gen_defers(ctx, ctx->scope);
	pop_scope(ctx);

	pushi(ctx->current, NULL, Q_JMP, &loop, NULL);

	push(&ctx->current->body, &endl);
}

static void
gen_expr_if(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value cond = {0};
	gen_temp(ctx, &cond, &qbe_word, "cond.%d");
	gen_expression(ctx, expr->_if.cond, &cond);

	struct qbe_statement tlabel = {0}, flabel = {0}, endl = {0};
	struct qbe_value tbranch = {0}, fbranch = {0}, end = {0};
	tbranch.kind = QV_LABEL;
	tbranch.name = strdup(genl(&tlabel, &ctx->id, "branch_true.%d"));
	fbranch.kind = QV_LABEL;
	fbranch.name = strdup(genl(&flabel, &ctx->id, "branch_false.%d"));
	end.name = strdup(genl(&endl, &ctx->id, "end.%d"));
	end.kind = QV_LABEL;

	pushi(ctx->current, NULL, Q_JNZ, &cond, &tbranch, &fbranch, NULL);

	push(&ctx->current->body, &tlabel);
	gen_expression(ctx, expr->_if.true_branch, out);
	if (!expr->_if.true_branch->terminates) {
		pushi(ctx->current, NULL, Q_JMP, &end, NULL);
	}

	push(&ctx->current->body, &flabel);
	if (expr->_if.false_branch) {
		gen_expression(ctx, expr->_if.false_branch, out);
		if (!expr->_if.false_branch->terminates) {
			pushi(ctx->current, NULL, Q_JMP, &end, NULL);
		}
	}

	push(&ctx->current->body, &endl);
}

static void
gen_expr_list(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_statement endl = {0};
	struct qbe_value end = {0};
	end.kind = QV_LABEL;
	end.name = strdup(genl(&endl, &ctx->id, "expr_list_end.%d"));
	push_scope(ctx, SCOPE_OTHER, &end);

	const struct expressions *exprs = &expr->list.exprs;
	while (exprs) {
		const struct qbe_value *dest = NULL;
		if (!exprs->next) {
			dest = out; // Last value determines expression result
		}
		gen_expression(ctx, exprs->expr, dest);
		exprs = exprs->next;
	}

	gen_defers(ctx, ctx->scope);
	pop_scope(ctx);
	push(&ctx->current->body, &endl);
}

static void
gen_match_tagged(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct type *mtype = expr->match.value->result;
	struct qbe_value mval = {0}, tag = {0}, match = {0}, temp = {0};
	// Kill me
	alloc_temp(ctx, &mval, mtype, "match.%d");
	qval_deref(&mval);
	gen_expression(ctx, expr->match.value, &mval);
	gen_temp(ctx, &temp, &qbe_word, "temp.%d");

	gen_temp(ctx, &tag, &qbe_word, "tag.%d");
	pushi(ctx->current, &tag, Q_LOADUW, &mval, NULL);

	struct qbe_statement olabel = {0};
	struct qbe_value obranch = {0};
	obranch.kind = QV_LABEL;
	obranch.name = strdup(genl(&olabel, &ctx->id, "out.%d"));

	struct match_case *_default = NULL;
	for (struct match_case *_case = expr->match.cases;
			_case; _case = _case->next) {
		if (!_case->type) {
			_default = _case;
			continue;
		}

		struct qbe_statement tlabel = {0}, flabel = {0};
		struct qbe_value tbranch = {0}, fbranch = {0};
		tbranch.kind = QV_LABEL;
		tbranch.name = strdup(genl(&tlabel, &ctx->id, "match.%d"));
		fbranch.kind = QV_LABEL;
		fbranch.name = strdup(genl(&flabel, &ctx->id, "next.case.%d"));

		const struct type_tagged_union *tu;
		struct type_tagged_union synthetic = {0};
		if (_case->type->storage != TYPE_STORAGE_TAGGED) {
			synthetic.type = _case->type;
			tu = &synthetic;
		} else {
			tu = &_case->type->tagged;
		}

		for (; tu; tu = tu->next) {
			struct qbe_statement nlabel = {0};
			struct qbe_value nbranch = {0};
			nbranch.kind = QV_LABEL;
			nbranch.name = strdup(genl(&nlabel, &ctx->id, "next.opt.%d"));

			constw(&match, tu->type->id);
			pushi(ctx->current, &temp, Q_CEQW, &match, &tag, NULL);
			pushi(ctx->current, NULL, Q_JNZ,
				&temp, &tbranch, &nbranch, NULL);
			push(&ctx->current->body, &nlabel);
		}

		pushi(ctx->current, NULL, Q_JMP, &fbranch, NULL);
		push(&ctx->current->body, &tlabel);

		if (_case->object) {
			struct qbe_value val = {0}, temp = {0};
			gen_temp(ctx, &val,
				qtype_for_type(ctx, _case->type, false),
				"bound.%d");
			struct gen_binding *binding =
				xcalloc(1, sizeof(struct gen_binding));
			binding->name = strdup(val.name);
			binding->object = _case->object;
			binding->next = ctx->bindings;
			ctx->bindings = binding;
			constl(&temp, mtype->align);
			val.type = &qbe_long;
			pushi(ctx->current, &val, Q_ADD, &mval, &temp, NULL);
		}

		gen_expression(ctx, _case->value, out);
		pushi(ctx->current, NULL, Q_JMP, &obranch, NULL);
		push(&ctx->current->body, &flabel);
	}

	if (_default) {
		gen_expression(ctx, _default->value, out);
	}

	push(&ctx->current->body, &olabel);
}

static void
gen_match_nullable(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value mval = {0}, temp = {0};
	gen_temp(ctx, &mval, &qbe_long, "match.%d"); // XXX: ARCH
	gen_temp(ctx, &temp, &qbe_long, "temp.%d"); // XXX: ARCH
	gen_expression(ctx, expr->match.value, &mval);

	struct qbe_statement olabel = {0};
	struct qbe_value obranch = {0};
	obranch.kind = QV_LABEL;
	obranch.name = strdup(genl(&olabel, &ctx->id, "out.%d"));

	struct match_case *_default = NULL;
	for (struct match_case *_case = expr->match.cases;
			_case; _case = _case->next) {
		if (!_case->type) {
			_default = _case;
			continue;
		}

		struct qbe_statement tlabel = {0}, flabel = {0};
		struct qbe_value tbranch = {0}, fbranch = {0};
		tbranch.kind = QV_LABEL;
		tbranch.name = strdup(genl(&tlabel, &ctx->id, "match.%d"));
		fbranch.kind = QV_LABEL;
		fbranch.name = strdup(genl(&flabel, &ctx->id, "next.case.%d"));

		struct qbe_value zero = {0};
		constl(&zero, 0);
		pushi(ctx->current, &temp, Q_CEQL, &mval, &zero, NULL);

		if (_case->type->storage == TYPE_STORAGE_NULL) {
			pushi(ctx->current, NULL, Q_JNZ,
				&temp, &tbranch, &fbranch, NULL);
		} else {
			pushi(ctx->current, NULL, Q_JNZ,
				&temp, &fbranch, &tbranch, NULL);
		}

		push(&ctx->current->body, &tlabel);

		if (_case->object) {
			struct qbe_value val = {0};
			alloc_temp(ctx, &val, _case->type, "bound.%d");
			struct gen_binding *binding =
				xcalloc(1, sizeof(struct gen_binding));
			binding->name = strdup(val.name);
			binding->object = _case->object;
			binding->next = ctx->bindings;
			ctx->bindings = binding;
			gen_store(ctx, &val, &mval);
		}

		gen_expression(ctx, _case->value, out);
		pushi(ctx->current, NULL, Q_JMP, &obranch, NULL);
		push(&ctx->current->body, &flabel);
	}

	if (_default) {
		gen_expression(ctx, _default->value, out);
	}

	push(&ctx->current->body, &olabel);
}

static void
gen_expr_match(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct type *mtype = type_dealias(expr->match.value->result);
	switch (mtype->storage) {
	case TYPE_STORAGE_TAGGED:
		gen_match_tagged(ctx, expr, out);
		break;
	case TYPE_STORAGE_POINTER:
		gen_match_nullable(ctx, expr, out);
		break;
	default:
		assert(0); // Invariant
	}
}

static void
gen_expr_measure(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value src = {0}, temp = {0}, ptr = {0};
	switch (expr->measure.op) {
	case M_LEN:
		switch (expr->measure.value->result->storage) {
		case TYPE_STORAGE_ARRAY:
			gen_temp(ctx, &temp,
				qtype_for_type(ctx, expr->result, false),
				"len.%d");
			constl(&temp, expr->measure.value->result->array.length);
			gen_store(ctx, out, &temp);
			break;
		case TYPE_STORAGE_SLICE:
		case TYPE_STORAGE_STRING:
			// My god this is a fucking mess
			alloc_temp(ctx, &src,
				expr->measure.value->result,
				"len.src.%d");
			qval_deref(&src);
			gen_temp(ctx, &ptr, &qbe_long, "len.ptr.%d");
			gen_expression(ctx, expr->measure.value, &src);
			constl(&temp, builtin_type_size.size);
			pushi(ctx->current, &ptr, Q_COPY, &src, NULL);
			pushi(ctx->current, &ptr, Q_ADD, &ptr, &temp, NULL);
			gen_temp(ctx, &temp, &qbe_long, "len.%d");
			pushi(ctx->current, &temp, Q_LOADL, &ptr, NULL);
			gen_store(ctx, out, &temp);
			break;
		default:
			assert(0); // Invariant
		}
		break;
	case M_SIZE:
		gen_temp(ctx, &temp,
			qtype_for_type(ctx, expr->result, false),
			"size.%d");
		constl(&temp, expr->measure.type->size);
		gen_store(ctx, out, &temp);
		break;
	case M_OFFSET:
		assert(0); // TODO
	}
}

static void
gen_expr_return(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (expr->_return.value) {
		gen_expression(ctx, expr->_return.value, ctx->return_value);
	}

	struct gen_scope_context *scope = ctx->scope;
	while (scope) {
		gen_defers(ctx, scope);
		if (scope->class == SCOPE_FUNC) {
			break;
		}
		scope = scope->parent;
	}
	assert(scope);

	pushi(ctx->current, NULL, Q_JMP, scope->end, NULL);
}

static void
gen_expr_slice(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	// XXX: ARCH
	struct qbe_value object = {0}, start = {0}, end = {0}, temp = {0};
	const struct type *otype = expr->slice.object->result;
	address_object(ctx, expr->slice.object, &temp);
	gen_temp(ctx, &object, object.type, "object.%d");
	object.type = temp.type;
	pushi(ctx->current, &object, Q_COPY, &temp, NULL);
	while (otype->storage == TYPE_STORAGE_POINTER) {
		pushi(ctx->current, &object, Q_LOADL, &object, NULL);
		otype = type_dereference(otype->pointer.referent);
	}

	gen_temp(ctx, &start, &qbe_long, "start.%d");
	gen_temp(ctx, &end, &qbe_long, "end.%d");

	struct qbe_value src = {0}, dest = {0}, offset = {0};
	gen_temp(ctx, &dest, &qbe_long, "dest.%d");
	gen_temp(ctx, &offset, &qbe_long, "offset.%d");

	if (expr->slice.start) {
		gen_expression(ctx, expr->slice.start, &start);
	} else {
		constl(&start, 0);
	}

	if (expr->slice.end) {
		gen_expression(ctx, expr->slice.end, &end);
	} else if (otype->storage == TYPE_STORAGE_ARRAY) {
		constl(&end, otype->array.length);
	} else {
		pushc(ctx->current, "load length");
		constl(&temp, 8); // XXX: ARCH
		pushi(ctx->current, &offset, Q_ADD, &object, &temp, NULL);
		pushi(ctx->current, &end, Q_LOADL, &offset, NULL);
	}

	// TODO: Bounds check
	pushi(ctx->current, &dest, Q_COPY, out, NULL);
	if (otype->storage == TYPE_STORAGE_SLICE) {
		pushc(ctx->current, "load array");

		gen_temp(ctx, &src, &qbe_long, "src.%d");
		pushi(ctx->current, &src, Q_LOADL, &object, NULL);

		pushc(ctx->current, "add offset");
		constl(&temp, otype->array.members->size);
		pushi(ctx->current, &offset, Q_MUL, &start, &temp, NULL);
		pushi(ctx->current, &offset, Q_ADD, &src, &offset, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &offset, &dest, NULL);

		pushc(ctx->current, "store length & capacity");
		constl(&temp, 8); // XXX: ARCH
		pushi(ctx->current, &offset, Q_SUB, &end, &start, NULL);
		constl(&temp, 8); // XXX: ARCH
		pushi(ctx->current, &dest, Q_ADD, &dest, &temp, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &offset, &dest, NULL);
		pushi(ctx->current, &dest, Q_ADD, &dest, &temp, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &offset, &dest, NULL);
	} else {
		gen_temp(ctx, &src, &qbe_long, "length.%d");

		constl(&temp, otype->array.members->size);
		pushi(ctx->current, &offset, Q_MUL, &start, &temp, NULL);
		pushi(ctx->current, &offset, Q_ADD, &object, &offset, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &offset, &dest, NULL);

		pushi(ctx->current, &offset, Q_SUB, &end, &start, NULL);

		constl(&temp, 8); // XXX: ARCH
		pushi(ctx->current, &dest, Q_ADD, &dest, &temp, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &offset, &dest, NULL);
		pushi(ctx->current, &dest, Q_ADD, &dest, &temp, NULL);
		pushi(ctx->current, NULL, Q_STOREL, &offset, &dest, NULL);
	}
}

static void
gen_expr_struct(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	// XXX: ARCH
	struct qbe_value base = {0}, ptr = {0}, offset = {0};
	gen_temp(ctx, &base, &qbe_long, "base.%d");
	gen_temp(ctx, &ptr, &qbe_long, "ptr.%d");
	pushi(ctx->current, &base, Q_COPY, out, NULL);

	const struct expression_struct *field = &expr->_struct;
	while (field) {
		constl(&offset, field->field->offset);
		ptr.type = &qbe_long;
		pushi(ctx->current, &ptr, Q_ADD, &base, &offset, NULL);

		ptr.type = qtype_for_type(ctx, field->field->type, true);
		ptr.indirect = !type_is_aggregate(field->field->type);
		gen_expression(ctx, field->value, &ptr);
		field = field->next;
	}
}

static void
gen_expr_switch(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	struct qbe_value sval = {0};
	gen_temp(ctx, &sval,
		qtype_for_type(ctx, expr->_switch.value->result, false),
		"switch.%d");
	gen_expression(ctx, expr->_switch.value, &sval);

	struct qbe_value match = {0}, temp = {0};
	gen_temp(ctx, &match,
		qtype_for_type(ctx, expr->_switch.value->result, false),
		"value.%d");
	gen_temp(ctx, &temp, &qbe_word, "temp.%d");
	struct qbe_statement olabel = {0};
	struct qbe_value obranch = {0};
	obranch.kind = QV_LABEL;
	obranch.name = strdup(genl(&olabel, &ctx->id, "out.%d"));

	struct switch_case *_default = NULL;
	for (struct switch_case *_case = expr->_switch.cases;
			_case; _case = _case->next) {
		if (!_case->options) {
			_default = _case;
			continue;
		}

		struct qbe_statement tlabel = {0}, flabel = {0};
		struct qbe_value tbranch = {0}, fbranch = {0};
		tbranch.kind = QV_LABEL;
		tbranch.name = strdup(genl(&tlabel, &ctx->id, "match.%d"));
		fbranch.kind = QV_LABEL;
		fbranch.name = strdup(genl(&flabel, &ctx->id, "next.case.%d"));

		for (struct case_option *opt = _case->options;
				opt; opt = opt->next) {
			struct qbe_statement nlabel = {0};
			struct qbe_value nbranch = {0};
			nbranch.kind = QV_LABEL;
			nbranch.name = strdup(genl(&nlabel, &ctx->id, "next.opt.%d"));

			gen_expr_constant(ctx, opt->value, &match);
			pushi(ctx->current, &temp, binarithm_for_op(
				BIN_LEQUAL, sval.type, sval.type->is_signed),
				&match, &sval, NULL);
			pushi(ctx->current, NULL, Q_JNZ,
				&temp, &tbranch, &nbranch, NULL);
			push(&ctx->current->body, &nlabel);
		}

		pushi(ctx->current, NULL, Q_JMP, &fbranch, NULL);
		push(&ctx->current->body, &tlabel);
		gen_expression(ctx, _case->value, out);
		pushi(ctx->current, NULL, Q_JMP, &obranch, NULL);
		push(&ctx->current->body, &flabel);
	}

	if (_default) {
		gen_expression(ctx, _default->value, out);
	}

	push(&ctx->current->body, &olabel);
}

static void
gen_expr_address(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	const struct expression *operand = expr->unarithm.operand;
	assert(operand->type == EXPR_ACCESS); // Invariant

	struct qbe_value src = {0};
	address_object(ctx, operand, &src);
	qval_address(&src);
	gen_store(ctx, out, &src);
}

static void
gen_expr_unarithm(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	if (expr->unarithm.op == UN_ADDRESS) {
		gen_expr_address(ctx, expr, out);
		return;
	}

	struct expression *operand = expr->unarithm.operand;
	struct qbe_value op = {0}, res = {0};
	gen_temp(ctx, &op, qtype_for_type(ctx, operand->result, false), "operand.%d");
	gen_temp(ctx, &res, qtype_for_type(ctx, expr->result, false), "result.%d");
	gen_expression(ctx, expr->unarithm.operand, &op);

	struct qbe_value temp = {0};
	temp.kind = QV_CONST;
	temp.type = op.type;
	switch (expr->unarithm.op) {
	case UN_LNOT:
		temp.lval = 1;
		pushi(ctx->current, &res, Q_XOR, &temp, &op, NULL);
		break;
	case UN_BNOT:
		temp.lval = (uint64_t)-1;
		pushi(ctx->current, &res, Q_XOR, &temp, &op, NULL);
		break;
	case UN_MINUS:
		temp.lval = 0;
		pushi(ctx->current, &res, Q_SUB, &temp, &op, NULL);
		break;
	case UN_PLUS:
		res = op; // no-op
		break;
	case UN_DEREF:
		if (!type_is_aggregate(expr->result)) {
			qval_deref(&op);
		}
		gen_load(ctx, &res, &op, type_is_signed(expr->result));
		break;
	case UN_ADDRESS:
		assert(0); // Invariant
	}

	gen_store(ctx, out, &res);
}

static void
gen_expression(struct gen_context *ctx,
	const struct expression *expr,
	const struct qbe_value *out)
{
	switch (expr->type) {
	case EXPR_ACCESS:
		gen_expr_access(ctx, expr, out);
		break;
	case EXPR_ALLOC:
		gen_expr_alloc(ctx, expr, out);
		break;
	case EXPR_ASSERT:
		gen_expr_assert(ctx, expr, out);
		break;
	case EXPR_ASSIGN:
		gen_expr_assign(ctx, expr, out);
		break;
	case EXPR_BINARITHM:
		gen_expr_binarithm(ctx, expr, out);
		break;
	case EXPR_BINDING:
		gen_expr_binding(ctx, expr, out);
		break;
	case EXPR_BREAK:
	case EXPR_CONTINUE:
		gen_expr_control(ctx, expr, out);
		break;
	case EXPR_CALL:
		gen_expr_call(ctx, expr, out);
		break;
	case EXPR_CAST:
		gen_expr_cast(ctx, expr, out);
		break;
	case EXPR_CONSTANT:
		gen_expr_constant(ctx, expr, out);
		break;
	case EXPR_DEFER:
		gen_expr_defer(ctx, expr, out);
		break;
	case EXPR_FOR:
		gen_expr_for(ctx, expr, out);
		break;
	case EXPR_IF:
		gen_expr_if(ctx, expr, out);
		break;
	case EXPR_LIST:
		gen_expr_list(ctx, expr, out);
		break;
	case EXPR_MATCH:
		gen_expr_match(ctx, expr, out);
		break;
	case EXPR_MEASURE:
		gen_expr_measure(ctx, expr, out);
		break;
	case EXPR_RETURN:
		gen_expr_return(ctx, expr, out);
		break;
	case EXPR_SLICE:
		gen_expr_slice(ctx, expr, out);
		break;
	case EXPR_STRUCT:
		gen_expr_struct(ctx, expr, out);
		break;
	case EXPR_SWITCH:
		gen_expr_switch(ctx, expr, out);
		break;
	case EXPR_UNARITHM:
		gen_expr_unarithm(ctx, expr, out);
		break;
	}
}

static void
gen_function_decl(struct gen_context *ctx, const struct declaration *decl)
{
	assert(decl->type == DECL_FUNC);
	const struct function_decl *func = &decl->func;
	const struct type *fntype = func->type;
	assert(!(func->flags & FN_TEST)); // TODO

	struct qbe_def *qdef = xcalloc(1, sizeof(struct qbe_def));
	qdef->kind = Q_FUNC;
	qdef->exported = decl->exported;
	qdef->name = decl->symbol ? strdup(decl->symbol) : ident_to_sym(&decl->ident);
	qdef->func.returns = qtype_for_type(ctx, fntype->func.result, false);
	ctx->current = &qdef->func;

	struct qbe_statement start_label = {0};
	genl(&start_label, &ctx->id, "start.%d");
	push(&qdef->func.prelude, &start_label);

	struct qbe_func_param *param, **next = &qdef->func.params;
	struct scope_object *obj = decl->func.scope->objects;
	while (obj) {
		param = *next = xcalloc(1, sizeof(struct qbe_func_param));
		assert(!obj->ident.ns); // Invariant
		param->name = strdup(obj->ident.name);
		param->type = qtype_for_type(ctx, obj->type, false);

		if (type_is_aggregate(obj->type)) {
			struct gen_binding *binding =
				xcalloc(1, sizeof(struct gen_binding));
			binding->name = strdup(param->name);
			binding->object = obj;
			binding->next = ctx->bindings;
			ctx->bindings = binding;
		} else {
			struct qbe_value val;
			binding_alloc(ctx, obj, &val, "param.%d");
			struct qbe_value src = {
				.kind = QV_TEMPORARY,
				.type = param->type,
				.name = param->name,
			};
			gen_store(ctx, &val, &src);
		}

		obj = obj->next;
		next = &param->next;
	}

	struct qbe_statement end_label = {0};
	struct qbe_value end_label_v = {
		.kind = QV_LABEL,
		.name = strdup(genl(&end_label, &ctx->id, "end.%d")),
	};
	ctx->end_label = &end_label_v;

	struct qbe_value rval = {0};
	if (fntype->func.result->storage != TYPE_STORAGE_VOID) {
		alloc_temp(ctx, &rval, fntype->func.result, "ret.%d");
		if (type_is_aggregate(fntype->func.result)) {
			rval.indirect = false;
		}
		ctx->return_value = &rval;
	} else {
		ctx->return_value = NULL;
	}

	push_scope(ctx, SCOPE_FUNC, &end_label_v);
	pushl(&qdef->func, &ctx->id, "body.%d");
	gen_expression(ctx, func->body, ctx->return_value);
	gen_defers(ctx, ctx->scope);
	push(&qdef->func.body, &end_label);
	pop_scope(ctx);

	if (fntype->func.result->storage != TYPE_STORAGE_VOID) {
		if (type_is_aggregate(fntype->func.result)) {
			pushi(&qdef->func, NULL, Q_RET, ctx->return_value, NULL);
		} else {
			struct qbe_value load = {0};
			gen_loadtemp(ctx, &load, ctx->return_value,
				qdef->func.returns,
				type_is_signed(fntype->func.result));
			pushi(&qdef->func, NULL, Q_RET, &load, NULL);
		}
	} else {
		pushi(&qdef->func, NULL, Q_RET, NULL);
	}

	// Free bindings
	struct gen_binding *binding = ctx->bindings;
	while (binding) {
		struct gen_binding *next = binding->next;
		free(binding->name);
		free(binding);
		binding = next;
	}
	ctx->bindings = NULL;

	qbe_append_def(ctx->out, qdef);
	ctx->current = NULL;

	if (func->flags & FN_INIT) {
		struct qbe_def *idef = xcalloc(1, sizeof(struct qbe_def));
		idef->kind = Q_DATA;
		int l = snprintf(NULL, 0, ".init.%s", qdef->name);
		idef->name = xcalloc(l + 1, 1);
		snprintf(idef->name, l + 1, ".init.%s", qdef->name);
		idef->data.align = 8;
		idef->data.section = strdup(".init_array");
		idef->data.items.type = QD_VALUE;
		idef->data.items.value.kind = QV_GLOBAL;
		idef->data.items.value.type = &qbe_long;
		idef->data.items.value.name = strdup(qdef->name);
		qbe_append_def(ctx->out, idef);
	}

	if (func->flags & FN_FINI) {
		struct qbe_def *fdef = xcalloc(1, sizeof(struct qbe_def));
		fdef->kind = Q_DATA;
		int l = snprintf(NULL, 0, ".fini.%s", qdef->name);
		fdef->name = xcalloc(l + 1, 1);
		snprintf(fdef->name, l + 1, ".fini.%s", qdef->name);
		fdef->data.align = 8;
		fdef->data.section = strdup(".fini_array");
		fdef->data.items.type = QD_VALUE;
		fdef->data.items.value.kind = QV_GLOBAL;
		fdef->data.items.value.type = &qbe_long;
		fdef->data.items.value.name = strdup(qdef->name);
		qbe_append_def(ctx->out, fdef);
	}
}

static void
gen_data_item(struct gen_context *ctx, struct expression *expr,
	struct qbe_data_item *item)
{
	assert(expr->type == EXPR_CONSTANT);

	struct qbe_def *def;
	const union expression_constant *constant = &expr->constant;
	const struct type *type = type_dealias(expr->result);
	switch (type->storage) {
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_U8:
		item->type = QD_VALUE;
		constw(&item->value, (uint8_t)constant->uval);
		item->value.type = &qbe_byte;
		break;
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
		item->type = QD_VALUE;
		constw(&item->value, (uint16_t)constant->uval);
		item->value.type = &qbe_half;
		break;
	case TYPE_STORAGE_BOOL:
		item->type = QD_VALUE;
		constw(&item->value, constant->bval ? 1 : 0);
		break;
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_RUNE:
		item->type = QD_VALUE;
		constw(&item->value, (uint32_t)constant->uval);
		break;
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_SIZE:
		item->type = QD_VALUE;
		constw(&item->value, (uint32_t)constant->uval);
		break;
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		assert(0); // TODO
	case TYPE_STORAGE_UINTPTR:
		assert(0); // TODO: What are the semantics for this?
	case TYPE_STORAGE_POINTER:
		assert(expr->type == EXPR_CONSTANT); // TODO?
		item->type = QD_VALUE;
		constl(&item->value, (uint64_t)constant->uval); // XXX: ARCH
		break;
	case TYPE_STORAGE_ARRAY:
		assert(type->array.length != SIZE_UNDEFINED);
		size_t n = type->array.length;
		for (struct array_constant *c = constant->array;
				c && n; c = c->next ? c->next : c, --n) {
			gen_data_item(ctx, c->value, item);
			while (item->next) {
				item = item->next;
			}
			if (n > 1 || c->next) {
				item->next = xcalloc(1,
					sizeof(struct qbe_data_item));
				item = item->next;
			}
		}
		break;
	case TYPE_STORAGE_STRING:
		def = xcalloc(1, sizeof(struct qbe_def));
		def->name = gen_name(ctx, "strdata.%d");
		def->kind = Q_DATA;
		def->data.items.type = QD_STRING;
		def->data.items.str = strdup(expr->constant.string.value);
		def->data.items.sz = expr->constant.string.len;

		item->type = QD_VALUE;
		if (expr->constant.string.len != 0) {
			qbe_append_def(ctx->out, def);
			item->value.kind = QV_GLOBAL;
			item->value.type = &qbe_long;
			item->value.name = strdup(def->name);
		} else {
			free(def);
			constl(&item->value, 0);
		}

		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		constl(&item->value, expr->constant.string.len);
		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		constl(&item->value, expr->constant.string.len);
		break;
	case TYPE_STORAGE_SLICE:
		def = xcalloc(1, sizeof(struct qbe_def));
		def->name = gen_name(ctx, "sldata.%d");
		def->kind = Q_DATA;

		struct qbe_data_item *subitem = &def->data.items;
		size_t len = 0;
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
			constl(&item->value, 0);
		}

		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		constl(&item->value, len);
		item->next = xcalloc(1, sizeof(struct qbe_data_item));
		item = item->next;
		item->type = QD_VALUE;
		constl(&item->value, len);
		break;
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_VOID:
		assert(0); // Invariant
	}
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
	case DECL_CONST:
		// Nothing to do
		break;
	case DECL_FUNC:
		gen_function_decl(ctx, decl);
		break;
	case DECL_GLOBAL:
		gen_global_decl(ctx, decl);
		break;
	case DECL_TYPE:
		// Nothing to do
		break;
	}
}

void
gen(const struct unit *unit, struct qbe_program *out)
{
	struct gen_context ctx = {
		.out = out,
		.ns = unit->ns,
	};
	ctx.out->next = &ctx.out->defs;
	const struct declarations *decls = unit->declarations;
	trenter(TR_GEN, "gen");
	while (decls) {
		gen_decl(&ctx, decls->decl);
		decls = decls->next;
	}
	trleave(TR_GEN, NULL);
}
