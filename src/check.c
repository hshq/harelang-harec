#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "check.h"
#include "eval.h"
#include "expr.h"
#include "mod.h"
#include "scope.h"
#include "tags.h"
#include "trace.h"
#include "type_store.h"
#include "types.h"
#include "util.h"

static void
mkident(struct context *ctx, struct identifier *out, const struct identifier *in)
{
	identifier_dup(out, in);
	if (ctx->ns) {
		out->ns = xcalloc(1, sizeof(struct identifier));
		identifier_dup(out->ns, ctx->ns);
	}
}

static void
expect(const struct location *loc, bool constraint, char *fmt, ...)
{
	if (!constraint) {
		va_list ap;
		va_start(ap, fmt);

		fprintf(stderr, "Error %s:%d:%d: ",
			loc->path, loc->lineno, loc->colno);
		vfprintf(stderr, fmt, ap);
		fprintf(stderr, "\n");
		abort();
	}
}

static struct expression *
lower_implicit_cast(const struct type *to, struct expression *expr)
{
	if (to == expr->result) {
		return expr;
	}

	if (type_dealias(to)->storage == STORAGE_TAGGED) {
		const struct type *interim =
			tagged_select_subtype(to, expr->result);
		if (interim) {
			expr = lower_implicit_cast(interim, expr);
		}
	}

	struct expression *cast = xcalloc(1, sizeof(struct expression));
	cast->type = EXPR_CAST;
	cast->result = to;
	cast->terminates = expr->terminates;
	cast->cast.kind = C_CAST;
	cast->cast.value = expr;
	return cast;
}

void check_expression(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint);

static void
check_expr_access(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trace(TR_CHECK, "access");
	expr->type = EXPR_ACCESS;
	expr->access.type = aexpr->access.type;

	const struct scope_object *obj;
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		obj = scope_lookup(ctx->scope, &aexpr->access.ident);
		char buf[1024];
		identifier_unparse_static(&aexpr->access.ident, buf, sizeof(buf));
		expect(&aexpr->loc, obj, "Unknown object '%s'", buf);
		switch (obj->otype) {
		case O_CONST:
			// Lower constants
			*expr = *obj->value;
			break;
		case O_BIND:
		case O_DECL:
			expr->result = obj->type;
			expr->access.object = obj;
			break;
		case O_TYPE:
			expect(&aexpr->loc,
				type_dealias(obj->type)->storage == STORAGE_VOID,
				"Cannot use non-void type alias '%s' as constant",
				identifier_unparse(&obj->type->alias.ident));
			expr->type = EXPR_CONSTANT;
			expr->result = obj->type;
			break;
		}
		break;
	case ACCESS_INDEX:
		expr->access.array = xcalloc(1, sizeof(struct expression));
		expr->access.index = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->access.array,
			expr->access.array, NULL);
		check_expression(ctx, aexpr->access.index,
			expr->access.index, NULL);
		const struct type *atype =
			type_dereference(expr->access.array->result);
		expect(&aexpr->access.array->loc, atype,
			"Cannot dereference nullable pointer for indexing");
		const struct type *itype =
			type_dealias(expr->access.index->result);
		expect(&aexpr->access.array->loc,
			atype->storage == STORAGE_ARRAY || atype->storage == STORAGE_SLICE,
			"Cannot index non-array, non-slice %s object",
			type_storage_unparse(atype->storage));
		expect(&aexpr->access.index->loc, type_is_integer(itype),
			"Cannot use non-integer %s type as slice/array index",
			type_storage_unparse(itype->storage));
		expr->access.index = lower_implicit_cast(
			&builtin_type_size, expr->access.index);
		expr->result = type_store_lookup_with_flags(ctx->store,
			atype->array.members, atype->flags | atype->array.members->flags);
		break;
	case ACCESS_FIELD:
		expr->access._struct = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->access._struct,
			expr->access._struct, NULL);
		const struct type *stype =
			type_dereference(expr->access._struct->result);
		expect(&aexpr->access._struct->loc, stype,
			"Cannot dereference nullable pointer for field selection");
		expect(&aexpr->access._struct->loc,
			stype->storage == STORAGE_STRUCT || stype->storage == STORAGE_UNION,
			"Cannot select field from non-struct, non-union object");
		expr->access.field = type_get_field(stype, aexpr->access.field);
		expect(&aexpr->access._struct->loc, expr->access.field,
			"No such struct field '%s'", aexpr->access.field);
		expr->result = expr->access.field->type;
		break;
	case ACCESS_TUPLE:
		expr->access.tuple = xcalloc(1, sizeof(struct expression));
		expr->access.value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->access.tuple,
			expr->access.tuple, NULL);
		check_expression(ctx, aexpr->access.value,
			expr->access.value, NULL);
		assert(expr->access.value->type == EXPR_CONSTANT);

		const struct type *ttype =
			type_dereference(expr->access.tuple->result);
		expect(&aexpr->access.tuple->loc, ttype,
			"Cannot dereference nullable pointer for value selection");
		expect(&aexpr->access.tuple->loc,
			ttype->storage == STORAGE_TUPLE,
			"Cannot select value from non-tuple object");
		expect(&aexpr->access.tuple->loc,
			type_is_integer(expr->access.value->result),
			"Cannot use non-integer constant to select tuple value");

		expr->access.tvalue = type_get_value(ttype,
			aexpr->access.value->constant.uval);
		expect(&aexpr->access.tuple->loc, expr->access.tvalue,
			"No such tuple value '%zu'",
			aexpr->access.value->constant.uval);

		expr->result = expr->access.tvalue->type;
		break;
	}
}

static void
check_expr_alloc(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	assert(aexpr->type == EXPR_ALLOC);
	trace(TR_CHECK, "alloc");
	expr->type = EXPR_ALLOC;
	expr->alloc.expr = xcalloc(sizeof(struct expression), 1);
	const struct type *inittype = NULL;
	if (hint && type_dealias(hint)->storage == STORAGE_POINTER) {
		inittype = type_dealias(hint)->pointer.referent;
	} else if (hint && type_dealias(hint)->storage == STORAGE_SLICE) {
		inittype = hint;
	}
	check_expression(ctx, aexpr->alloc.expr, expr->alloc.expr, inittype);

	enum type_storage storage =
		type_dealias(expr->alloc.expr->result)->storage;
	if (!hint && storage == STORAGE_SLICE) {
		hint = expr->alloc.expr->result;
	} else if (hint) {
		storage = type_dealias(hint)->storage;
	} else {
		hint = type_store_lookup_pointer(ctx->store,
			expr->alloc.expr->result, 0);
	}
	expr->result = hint;
	storage = type_dealias(hint)->storage;

	switch (storage) {
	case STORAGE_POINTER:
		if (aexpr->alloc.cap != NULL) {
			// We can't just expect(aexpr->alloc.cap != NULL)
			// because we want to use aexpr->alloc.cap->loc
			expect(&aexpr->alloc.cap->loc, false,
				"Allocation with capacity must be of slice type, not %s",
				type_storage_unparse(storage));
		}
		break;
	case STORAGE_SLICE:
		if (aexpr->alloc.cap != NULL) {
			expr->alloc.cap = xcalloc(sizeof(struct expression), 1);
			check_expression(ctx, aexpr->alloc.cap, expr->alloc.cap,
				&builtin_type_size);
			expect(&aexpr->alloc.cap->loc,
				type_is_assignable(&builtin_type_size,
					expr->alloc.cap->result),
				"Allocation capacity must be assignable to size");
		}
		break;
	default:
		expect(&aexpr->loc, false,
			"Allocation type must be pointer or slice, not %s",
			type_storage_unparse(storage));
	}
}

static void
check_expr_append(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	assert(aexpr->type == EXPR_APPEND);
	trace(TR_CHECK, "append");
	expr->type = EXPR_APPEND;
	expr->result = &builtin_type_void;
	expr->append.expr = xcalloc(sizeof(struct expression), 1);
	check_expression(ctx, aexpr->append.expr, expr->append.expr, NULL);
	expect(&aexpr->append.expr->loc,
		type_dealias(expr->append.expr->result)->storage == STORAGE_SLICE,
		"append must operate on a slice");
	expect(&aexpr->append.expr->loc,
		!(type_dealias(expr->append.expr->result)->flags & TYPE_CONST),
		"append must operate on a mutable slice");
	expect(&aexpr->append.expr->loc,
		expr->append.expr->type == EXPR_ACCESS
		|| (expr->append.expr->type == EXPR_UNARITHM
			&& expr->append.expr->unarithm.op == UN_DEREF),
		"append must operate on a slice object");
	const struct type *memb =
		type_dealias(expr->append.expr->result)->array.members;
	struct append_values **next = &expr->append.values;
	for (struct ast_append_values *avalue = aexpr->append.values; avalue;
			avalue = avalue->next) {
		struct append_values *value = *next =
			xcalloc(sizeof(struct append_values), 1);
		value->expr = 
			xcalloc(sizeof(struct expression), 1);
		check_expression(ctx, avalue->expr, value->expr, memb);
		expect(&avalue->expr->loc,
			type_is_assignable(memb, value->expr->result),
			"appended value must be assignable to member type");
		value->expr = lower_implicit_cast(memb, value->expr);
		next = &value->next;
	}
	if (aexpr->append.variadic != NULL) {
		const struct type *type = expr->append.expr->result;
		expr->append.variadic = xcalloc(sizeof(struct expression), 1);
		check_expression(ctx, aexpr->append.variadic,
			expr->append.variadic, type);
		expect(&aexpr->append.variadic->loc,
			type_is_assignable(type, expr->append.variadic->result),
			"appended slice must be assignable to slice type");
		expr->append.variadic =
			lower_implicit_cast(type, expr->append.variadic);
	}
}

static void
check_expr_assert(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trace(TR_CHECK, "assert");
	expr->type = EXPR_ASSERT;
	expr->result = &builtin_type_void;
	expr->assert.is_static = aexpr->assert.is_static;

	if (aexpr->assert.cond != NULL) {
		expr->assert.cond = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->assert.cond,
			expr->assert.cond, &builtin_type_bool);
		expect(&aexpr->assert.cond->loc,
			expr->assert.cond->result->storage == STORAGE_BOOL,
			"Assertion condition must be boolean");
	} else {
		expr->terminates = true;
	}

	expr->assert.message = xcalloc(1, sizeof(struct expression));
	if (aexpr->assert.message != NULL) {
		check_expression(ctx, aexpr->assert.message,
			expr->assert.message, &builtin_type_str);
		expect(&aexpr->assert.message->loc,
			expr->assert.message->result->storage == STORAGE_STRING,
			"Assertion message must be string");
	} else {
		int n = snprintf(NULL, 0, "Assertion failed: %s:%d:%d",
			aexpr->loc.path, aexpr->loc.lineno, aexpr->loc.colno);
		char *s = xcalloc(1, n + 1);
		snprintf(s, n, "Assertion failed: %s:%d:%d",
			aexpr->loc.path, aexpr->loc.lineno, aexpr->loc.colno);

		expr->assert.message->type = EXPR_CONSTANT;
		expr->assert.message->result = &builtin_type_const_str;
		expr->assert.message->constant.string.value = s;
		expr->assert.message->constant.string.len = n;
	}

	if (expr->assert.is_static) {
		bool cond;
		if (expr->assert.cond != NULL) {
			struct expression out = {0};
			enum eval_result r =
				eval_expr(ctx, expr->assert.cond, &out);
			expect(&aexpr->assert.cond->loc, r == EVAL_OK,
				"Unable to evaluate static assertion at compile time");
			assert(out.result->storage == STORAGE_BOOL);
			cond = out.constant.bval;
		} else {
			cond = false; 
		}
		if (aexpr->assert.message != NULL) {
			expect(&aexpr->assert.cond->loc, cond,
				"Static assertion failed: %s",
				expr->assert.message->constant.string.value);
		} else {
			expect(&aexpr->loc, cond, "Static assertion failed");
		}
	}
}

static void
check_expr_assign(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trace(TR_CHECK, "assign");
	expr->type = EXPR_ASSIGN;
	expr->result = &builtin_type_void;
	expr->assign.indirect = aexpr->assign.indirect;
	struct expression *object = xcalloc(1, sizeof(struct expression));
	struct expression *value = xcalloc(1, sizeof(struct expression));

	check_expression(ctx, aexpr->assign.object, object, NULL);

	expr->assign.op = aexpr->assign.op;

	if (aexpr->assign.indirect) {
		expect(&aexpr->loc,
			object->result->storage == STORAGE_POINTER,
			"Cannot dereference non-pointer type for assignment");
		expect(&aexpr->loc,
			!(object->result->pointer.flags & PTR_NULLABLE),
			"Cannot dereference nullable pointer type");
		check_expression(ctx, aexpr->assign.value, value,
			object->result->pointer.referent);
		expect(&aexpr->loc,
			type_is_assignable(object->result->pointer.referent,
				value->result),
			"Value type is not assignable to pointer type");
		value = lower_implicit_cast(object->result->pointer.referent, value);
	} else {
		check_expression(ctx, aexpr->assign.value, value, object->result);
		assert(object->type == EXPR_ACCESS
				|| object->type == EXPR_SLICE); // Invariant
		if (object->type == EXPR_SLICE) {
			expect(&aexpr->assign.object->loc,
				expr->assign.op == BIN_LEQUAL,
				"Slice assignments may not have a binop");
		}
		expect(&aexpr->loc, !(object->result->flags & TYPE_CONST),
				"Cannot assign to const object");
		expect(&aexpr->loc,
			type_is_assignable(object->result, value->result),
			"rvalue type is not assignable to lvalue");
		value = lower_implicit_cast(object->result, value);
	}

	expr->assign.object = object;
	expr->assign.value = value;
}

static const struct type *
type_promote(struct type_store *store,
	const struct type *_a,
	const struct type *_b)
{
	bool is_const = (_a->flags & TYPE_CONST) || (_b->flags & TYPE_CONST);
	const struct type *a = type_store_lookup_with_flags(store, _a, 0);
	const struct type *b = type_store_lookup_with_flags(store, _b, 0);

	if (a->storage == STORAGE_ALIAS) {
		return a == b || a->alias.type == b ? _b : NULL;
	}
	if (b->storage == STORAGE_ALIAS) {
		return a == b || a == b->alias.type ? _a : NULL;
	}

	const struct type *base = a == b ? a : NULL;

	switch (a->storage) {
	case STORAGE_ARRAY:
		if (a->array.length == SIZE_UNDEFINED && a->array.members) {
			base = b;
			break;
		}
		if (b->array.length == SIZE_UNDEFINED && b->array.members) {
			base = a;
			break;
		}
		break;
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
		if (!type_is_integer(b) || !type_is_signed(b)
				|| b->size == a->size) {
			break;
		}
		base = a->size > b->size ? a : b;
		break;
	case STORAGE_U32:
	case STORAGE_U16:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_CHAR:
		if (!type_is_integer(b) || type_is_signed(b)
				|| b->size == a->size) {
			break;
		}
		base = a->size > b->size ? a : b;
		break;
	case STORAGE_F32:
	case STORAGE_F64:
		if (!type_is_float(b) || b->size == a->size) {
			break;
		}
		base = a->size > b->size ? a : b;
		break;
	case STORAGE_POINTER:
		if (b->storage == STORAGE_NULL) {
			base = a;
			break;
		}
		if (b->storage != STORAGE_POINTER) {
			break;
		}
		base = type_promote(store, a->pointer.referent,
			b->pointer.referent);
		if (base) {
			base = type_store_lookup_pointer(store, base,
				a->pointer.flags | b->pointer.flags);
		}
		break;
	case STORAGE_NULL:
		if (b->storage == STORAGE_POINTER
				|| b->storage == STORAGE_NULL) {
			base = b;
		}
		break;
	// Cannot be promoted
	case STORAGE_BOOL:
	case STORAGE_ENUM:
	case STORAGE_FUNCTION:
	case STORAGE_RUNE:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UINTPTR:
	case STORAGE_UNION:
	case STORAGE_VOID:
		break;
	// Handled above
	case STORAGE_ALIAS:
		break;
	// Invariant
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		assert(0);
	}

	if (is_const && base) {
		base = type_store_lookup_with_flags(store, base,
			base->flags | TYPE_CONST);
	}
	return base;
}

static bool
aexpr_is_flexible(const struct ast_expression *expr)
{
	return expr->type == EXPR_CONSTANT
		&& storage_is_flexible(expr->constant.storage);
}

static void
check_expr_binarithm(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trace(TR_CHECK, "binarithm");
	expr->type = EXPR_BINARITHM;
	expr->binarithm.op = aexpr->binarithm.op;

	bool numeric;
	switch (expr->binarithm.op) {
	// Numeric arithmetic
	case BIN_BAND:
	case BIN_BOR:
	case BIN_DIV:
	case BIN_LSHIFT:
	case BIN_MINUS:
	case BIN_MODULO:
	case BIN_PLUS:
	case BIN_RSHIFT:
	case BIN_TIMES:
	case BIN_BXOR:
		numeric = true;
		break;
	// Logical arithmetic
	case BIN_GREATER:
	case BIN_GREATEREQ:
	case BIN_LAND:
	case BIN_LEQUAL:
	case BIN_LESS:
	case BIN_LESSEQ:
	case BIN_LOR:
	case BIN_LXOR:
	case BIN_NEQUAL:
		numeric = false;
		hint = NULL;
		break;
	}

	bool lflex = aexpr_is_flexible(aexpr->binarithm.lvalue),
		rflex = aexpr_is_flexible(aexpr->binarithm.rvalue);
	struct expression *lvalue = xcalloc(1, sizeof(struct expression)),
		*rvalue = xcalloc(1, sizeof(struct expression));

	if (hint && lflex && rflex) {
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue, hint);
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue, hint);
	} else if (lflex && rflex) {
		intmax_t l = aexpr->binarithm.lvalue->constant.ival,
			r = aexpr->binarithm.rvalue->constant.ival,
			max = l > r ? l : r, min = l < r ? l : r;
		enum type_storage storage = STORAGE_ICONST;
		if (min < 0) {
			if (max < ((intmax_t)1 << 7) - 1
					&& min > -((intmax_t)1 << 8)) {
				storage = STORAGE_I8;
			} else if (max < ((intmax_t)1 << 15) - 1
					&& min > -((intmax_t)1 << 16)) {
				storage = STORAGE_I16;
			} else if (max < ((intmax_t)1 << 31) - 1
					&& min > -((intmax_t)1 << 32)) {
				storage = STORAGE_I32;
			} else {
				storage = STORAGE_I64;
			}
		} else {
			if (max < ((intmax_t)1 << 8)) {
				storage = STORAGE_U8;
			} else if (max < ((intmax_t)1 << 16)) {
				storage = STORAGE_U16;
			} else if (max < ((intmax_t)1 << 32)) {
				storage = STORAGE_U32;
			} else {
				storage = STORAGE_U64;
			}
		}
		assert(storage != STORAGE_ICONST);
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue,
			builtin_type_for_storage(storage, false));
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue,
			builtin_type_for_storage(storage, false));
	} else if (!lflex && rflex) {
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue, hint);
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue,
			lvalue->result);
	} else if (lflex && !rflex) {
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue, hint);
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue,
			rvalue->result);
	} else {
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue, hint);
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue, hint);
	}

	expr->binarithm.lvalue = lvalue;
	expr->binarithm.rvalue = rvalue;

	const struct type *p =
		type_promote(ctx->store, lvalue->result, rvalue->result);
	expect(&aexpr->loc, p != NULL, "Cannot promote lvalue and rvalue");
	lvalue = lower_implicit_cast(p, lvalue);
	rvalue = lower_implicit_cast(p, rvalue);
	if (numeric) {
		expr->result = p;
	} else {
		expr->result = &builtin_type_bool;
	}
}

static void
check_expr_binding(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trace(TR_CHECK, "binding");
	expr->type = EXPR_BINDING;
	expr->result = &builtin_type_void;

	struct expression_binding *binding = &expr->binding;
	struct expression_binding **next = &expr->binding.next;

	const struct ast_expression_binding *abinding = &aexpr->binding;
	while (abinding) {
		const struct type *type = NULL;
		if (abinding->type) {
			type = type_store_lookup_atype(
				ctx->store, abinding->type);
			type = type_store_lookup_with_flags(ctx->store,
				type, type->flags | abinding->flags);
		}

		struct identifier ident = {
			.name = abinding->name,
		};
		struct expression *initializer =
			xcalloc(1, sizeof(struct expression));

		struct identifier gen = {0};
		if (abinding->is_static) {
			// Generate a static declaration identifier
			int n = snprintf(NULL, 0, "static.%d", ctx->id);
			gen.name = xcalloc(n + 1, 1);
			snprintf(gen.name, n + 1, "static.%d", ctx->id);
			++ctx->id;
		}

		bool context = abinding->type
			&& abinding->type->storage == STORAGE_ARRAY
			&& abinding->type->array.contextual;
		if (type && !context) {
			// If the type is defined in advance, we can insert the
			// object into the scope early, which is required for
			// self-referencing objects.
			if (!abinding->is_static) {
				binding->object = scope_insert(ctx->scope,
					O_BIND, &ident, &ident, type, NULL);
			} else {
				binding->object = scope_insert(ctx->scope,
					O_DECL, &gen, &ident, type, NULL);
			}
		}

		check_expression(ctx, abinding->initializer, initializer, type);

		if (context) {
			expect(&aexpr->loc,
				initializer->result->storage == STORAGE_ARRAY,
				"Cannot infer array length from non-array type");
			expect(&aexpr->loc,
				initializer->result->array.members == type->array.members,
				"Initializer is not assignable to binding type");
			type = initializer->result;
		}

		if (context || !type) {
			if (!type) {
				type = type_store_lookup_with_flags(ctx->store,
					initializer->result, abinding->flags);
			}

			if (!abinding->is_static) {
				binding->object = scope_insert(ctx->scope,
					O_BIND, &ident, &ident, type, NULL);
			} else {
				binding->object = scope_insert(ctx->scope,
					O_DECL, &gen, &ident, type, NULL);
			}
		}

		expect(&aexpr->loc,
			type->size != 0 && type->size != SIZE_UNDEFINED,
			"Cannot create binding for type of zero or undefined size");
		expect(&aexpr->loc,
			type_is_assignable(type, initializer->result),
			"Initializer is not assignable to binding type");
		binding->initializer = lower_implicit_cast(type, initializer);

		if (abinding->is_static) {
			struct expression *value =
				xcalloc(1, sizeof(struct expression));
			enum eval_result r = eval_expr(
				ctx, binding->initializer, value);
			expect(&abinding->initializer->loc, r == EVAL_OK,
				"Unable to evaluate static initializer at compile time");
			// TODO: Free initializer
			binding->initializer = value;
		}

		if (abinding->next) {
			binding = *next =
				xcalloc(1, sizeof(struct expression_binding));
			next = &binding->next;
		}

		abinding = abinding->next;
	}
}

// Lower Hare-style variadic arguments into an array literal
static void
lower_vaargs(struct context *ctx,
	const struct ast_call_argument *aarg,
	struct expression *vaargs,
	const struct type *type)
{
	struct ast_expression val = {
		.type = EXPR_CONSTANT,
		.constant = {
			.storage = STORAGE_ARRAY,
		},
	};
	// TODO: Provide location some other way
	if (aarg) {
		val.loc = aarg->value->loc;
	}
	struct ast_array_constant **next = &val.constant.array;
	while (aarg) {
		struct ast_array_constant *item = *next =
			xcalloc(1, sizeof(struct ast_array_constant));
		item->value = aarg->value;
		aarg = aarg->next;
		next = &item->next;
	}

	// XXX: This error handling is minimum-effort and bad
	const struct type *hint = type_store_lookup_array(
		ctx->store, type, SIZE_UNDEFINED);
	check_expression(ctx, &val, vaargs, hint);
	assert(vaargs->result->storage == STORAGE_ARRAY);
	expect(&val.loc, vaargs->result->array.members == type,
		"Argument is not assignable to variadic parameter type");

	struct ast_array_constant *item = val.constant.array;
	while (item) {
		struct ast_array_constant *next = item->next;
		free(item);
		item = next;
	}
}

static void
check_expr_call(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "call");
	expr->type = EXPR_CALL;

	struct expression *lvalue = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->call.lvalue, lvalue, NULL);
	expr->call.lvalue = lvalue;

	const struct type *fntype = type_dereference(lvalue->result);
	expect(&aexpr->loc, fntype,
		"Cannot dereference nullable pointer type for function call");
	expect(&aexpr->loc,
		fntype->storage == STORAGE_FUNCTION,
		"Cannot call non-function type");
	expr->result = fntype->func.result;
	if (fntype->func.flags & FN_NORETURN) {
		expr->terminates = true;
	}

	struct call_argument *arg, **next = &expr->call.args;
	struct ast_call_argument *aarg = aexpr->call.args;
	struct type_func_param *param = fntype->func.params;
	while (param && aarg) {
		trenter(TR_CHECK, "arg");
		arg = *next = xcalloc(1, sizeof(struct call_argument));
		arg->value = xcalloc(1, sizeof(struct expression));

		if (!param->next && fntype->func.variadism == VARIADISM_HARE
				&& !aarg->variadic) {
			lower_vaargs(ctx, aarg, arg->value,
				param->type->array.members);
			arg->value = lower_implicit_cast(param->type, arg->value);
			param = NULL;
			aarg = NULL;
			trleave(TR_CHECK, NULL);
			break;
		}

		check_expression(ctx, aarg->value, arg->value, param->type);

		expect(&aarg->value->loc,
			type_is_assignable(param->type, arg->value->result),
			"Argument is not assignable to parameter type");
		arg->value = lower_implicit_cast(param->type, arg->value);

		aarg = aarg->next;
		param = param->next;
		next = &arg->next;
		trleave(TR_CHECK, NULL);
	}

	if (param && fntype->func.variadism == VARIADISM_HARE) {
		// No variadic arguments, lower to empty slice
		arg = *next = xcalloc(1, sizeof(struct call_argument));
		arg->value = xcalloc(1, sizeof(struct expression));
		lower_vaargs(ctx, NULL, arg->value, param->type->array.members);
		arg->value = lower_implicit_cast(param->type, arg->value);
		param = param->next;
	}

	expect(&aexpr->loc, !aarg, "Too many parameters for function call");
	expect(&aexpr->loc, !param, "Not enough parameters for function call");

	trleave(TR_CHECK, NULL);
}

static void
check_expr_cast(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trace(TR_CHECK, "cast");
	expr->type = EXPR_CAST;
	expr->cast.kind = aexpr->cast.kind;
	struct expression *value = expr->cast.value =
		xcalloc(1, sizeof(struct expression));
	const struct type *secondary = expr->cast.secondary =
		type_store_lookup_atype(ctx->store, aexpr->cast.type);
	check_expression(ctx, aexpr->cast.value, value, secondary);

	if (aexpr->cast.kind == C_ASSERTION || aexpr->cast.kind == C_TEST) {
		const struct type *primary = type_dealias(expr->cast.value->result);
		expect(&aexpr->cast.value->loc,
			primary->storage == STORAGE_TAGGED,
			"Expected a tagged union type");
		expect(&aexpr->cast.type->loc,
			type_is_castable(value->result, secondary),
			"Invalid cast");
		bool found = false;
		for (const struct type_tagged_union *t = &primary->tagged;
				t; t = t->next) {
			if (t->type->id == secondary->id) {
				found = true;
				break;
			}
		}
		expect(&aexpr->cast.type->loc, found,
			"Type is not a valid member of the tagged union type");
	}

	switch (aexpr->cast.kind) {
	case C_CAST:
		expect(&aexpr->cast.type->loc,
			type_is_castable(secondary, value->result),
			"Invalid cast");
		// Fallthrough
	case C_ASSERTION:
		expr->result = secondary;
		break;
	case C_TEST:
		expr->result = &builtin_type_bool;
		break;
	}
}

static void
check_expr_array(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	size_t len = 0;
	bool expandable = false;
	struct ast_array_constant *item = aexpr->constant.array;
	struct array_constant *cur, **next = &expr->constant.array;
	const struct type *type = NULL;
	if (hint) {
		hint = type_dealias(hint);
		if (hint->storage == STORAGE_ARRAY
				|| hint->storage == STORAGE_SLICE) {
			type = hint->array.members;
		} else if (hint->storage == STORAGE_TAGGED) {
			size_t narray = 0;
			for (const struct type_tagged_union *tu = &hint->tagged;
					tu; tu = tu->next) {
				const struct type *t = type_dealias(tu->type);
				if (t->storage == STORAGE_ARRAY
						|| t->storage == STORAGE_SLICE) {
					hint = t;
					type = hint->array.members;
					++narray;
				}
			}
			if (narray != 1) {
				type = hint = NULL;
			}
		} else {
			hint = NULL;
		}
	}

	while (item) {
		struct expression *value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, item->value, value, type);
		cur = *next = xcalloc(1, sizeof(struct array_constant));
		cur->value = value;

		if (!type) {
			type = value->result;
		} else {
			expect(&item->value->loc,
				type_is_assignable(type, value->result),
				"Array members must be of a uniform type");
			cur->value = lower_implicit_cast(type, cur->value);
		}

		if (item->expand) {
			expandable = true;
			expr->constant.array->expand = true;
			assert(!item->next);
		}

		item = item->next;
		next = &cur->next;
		++len;
	}

	if (expandable) {
		expect(&aexpr->loc, hint != NULL,
			"Cannot expand array for inferred type");
		expect(&aexpr->loc, hint->storage == STORAGE_ARRAY
				&& hint->array.length != SIZE_UNDEFINED
				&& hint->array.length >= len,
			"Cannot expand array into destination type");
		expr->result = type_store_lookup_array(ctx->store,
				type, hint->array.length);
	} else {
		expect(&aexpr->loc, type != NULL,
			"Cannot infer array type from context, try casting it to the desired type");
		expr->result = type_store_lookup_array(ctx->store, type, len);
	}
}

static const struct type *
lower_constant(const struct type *type, struct expression *expr)
{
	assert(expr->type == EXPR_CONSTANT);
	type = type_dealias(type);
	if (type_is_float(type)) {
		assert(0); // TODO
	}
	if (type->storage == STORAGE_TAGGED) {
		const struct type *tag = NULL;
		for (const struct type_tagged_union *tu = &type->tagged; tu;
				tu = tu->next) {
			if (lower_constant(tu->type, expr)) {
				if (tag != NULL) {
					// Ambiguous
					return NULL;
				}
				tag = tu->type;
			}
		}
		return tag;
	}
	if (!type_is_integer(type)) {
		return NULL;
	}
	if (type_is_signed(type)) {
		intmax_t max, min;
		switch (type->size) {
		case 1:
			max = INT8_MAX;
			min = INT8_MIN;
			break;
		case 2:
			max = INT16_MAX;
			min = INT16_MIN;
			break;
		case 4:
			max = INT32_MAX;
			min = INT32_MIN;
			break;
		case 8:
			max = INT64_MAX;
			min = INT64_MIN;
			break;
		default:
			assert(0);
		}
		if (expr->constant.ival <= max && expr->constant.ival >= min) {
			return type;
		}
		return NULL;
	}
	uintmax_t max;
	switch (type->size) {
	case 1:
		max = UINT8_MAX;
		break;
	case 2:
		max = UINT16_MAX;
		break;
	case 4:
		max = UINT32_MAX;
		break;
	case 8:
		max = UINT64_MAX;
		break;
	default:
		assert(0);
	}
	if (expr->constant.uval <= max) {
		return type;
	}
	return NULL;
}

static void
check_expr_constant(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trace(TR_CHECK, "constant");
	expr->type = EXPR_CONSTANT;
	expr->result = builtin_type_for_storage(aexpr->constant.storage, false);

	if (expr->result && expr->result->storage == STORAGE_ICONST) {
		if (hint == NULL) {
			hint = builtin_type_for_storage(STORAGE_INT, false);
		}
		expr->constant.ival = aexpr->constant.ival;
		const struct type *type = lower_constant(hint, expr);
		// TODO: This error message is awful
		expect(&aexpr->loc, type, "Integer constant out of range");
		expr->result = type;
	}

	switch (aexpr->constant.storage) {
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_ICONST:
	case STORAGE_INT:
		expr->constant.ival = aexpr->constant.ival;
		break;
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_SIZE:
		expr->constant.uval = aexpr->constant.uval;
		break;
	case STORAGE_RUNE:
		expr->constant.rune = aexpr->constant.rune;
		break;
	case STORAGE_BOOL:
		expr->constant.bval = aexpr->constant.bval;
		break;
	case STORAGE_NULL:
	case STORAGE_VOID:
		// No storage
		break;
	case STORAGE_ARRAY:
		check_expr_array(ctx, aexpr, expr, hint);
		break;
	case STORAGE_STRING:
		expr->constant.string.len = aexpr->constant.string.len;
		expr->constant.string.value = xcalloc(1, aexpr->constant.string.len);
		memcpy(expr->constant.string.value, aexpr->constant.string.value,
			aexpr->constant.string.len);
		break;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		assert(0); // TODO
	case STORAGE_CHAR:
	case STORAGE_ENUM:
	case STORAGE_UINTPTR:
	case STORAGE_ALIAS:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		assert(0); // Invariant
	}
}

static void
check_expr_defer(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expect(&aexpr->loc, !ctx->deferring,
		"Cannot defer within another defer expression.");
	expr->type = EXPR_DEFER;
	expr->result = &builtin_type_void;
	expr->defer.deferred = xcalloc(1, sizeof(struct expression));
	ctx->deferring = true;
	check_expression(ctx, aexpr->defer.deferred, expr->defer.deferred, NULL);
	ctx->deferring = false;
}

static void
check_expr_delete(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_DELETE;
	expr->result = &builtin_type_void;
	struct expression *dexpr = expr->delete.expr =
		xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->delete.expr, expr->delete.expr, NULL);
	const struct type *otype = NULL;
	switch (dexpr->type) {
	case EXPR_SLICE:
		otype = dexpr->slice.object->result;
		break;
	case EXPR_ACCESS:
		expect(&aexpr->delete.expr->loc, dexpr->access.type == ACCESS_INDEX,
			"Deleted expression must be slicing or indexing expression");
		otype = dexpr->access.array->result;
		break;
	default:
		expect(&aexpr->delete.expr->loc, false,
			"Deleted expression must be slicing or indexing expression");
	}
	otype = type_dealias(otype);
	while (otype->storage == STORAGE_POINTER) {
		otype = type_dealias(otype->pointer.referent);
	}
	expect(&aexpr->delete.expr->loc, otype->storage == STORAGE_SLICE,
		"delete must operate on a slice");
	expect(&aexpr->delete.expr->loc, !(otype->flags & TYPE_CONST),
		"delete must operate on a mutable slice");
}

static void
check_expr_control(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "control");
	expr->type = aexpr->type;
	expr->result = &builtin_type_void;
	expr->terminates = true;
	char *label = expr->control.label = aexpr->control.label;

	struct scope *scope = ctx->scope;
	for (; scope != NULL; scope = scope->parent) {
		if (scope->type != EXPR_FOR) {
			continue;
		}
		if (label == NULL) {
			break;
		}
		if (scope->label != NULL && strcmp(label, scope->label) == 0) {
			break;
		}
	}
	expect(&aexpr->loc, scope != NULL, "Unknown label %s",
		expr->control.label);
	trleave(TR_CHECK, NULL);
}

static void
check_expr_for(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "if");
	expr->type = EXPR_FOR;
	expr->result = &builtin_type_void;

	if (aexpr->_for.label) {
		expr->_for.label = strdup(aexpr->_for.label);
	}

	struct scope *scope = scope_push(&ctx->scope, TR_CHECK);
	expr->_for.scope = scope;
	scope->type = expr->type;
	scope->label = expr->_for.label;
	if (expr->_for.label) {
		for (scope = scope->parent; scope; scope = scope->parent) {
			if (scope->label == NULL) {
				continue;
			}
			expect(&aexpr->_for.label_loc,
				strcmp(scope->label, expr->_for.label) != 0,
				"for loop label must be unique among its ancestors");
		}
	}

	struct expression *bindings = NULL,
		*cond = NULL, *afterthought = NULL, *body = NULL;

	if (aexpr->_for.bindings) {
		bindings = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->_for.bindings, bindings, NULL);
		expr->_for.bindings = bindings;
	}

	cond = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_for.cond, cond, &builtin_type_bool);
	expr->_for.cond = cond;
	expect(&aexpr->_for.cond->loc,
		cond->result->storage == STORAGE_BOOL,
		"Expected for condition to be boolean");

	if (aexpr->_for.afterthought) {
		afterthought = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->_for.afterthought,
			afterthought, NULL);
		expr->_for.afterthought = afterthought;
	}

	body = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_for.body, body, NULL);
	expr->_for.body = body;

	scope_pop(&ctx->scope, TR_CHECK);
	trleave(TR_CHECK, NULL);
}

static void
check_expr_free(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	assert(aexpr->type == EXPR_FREE);
	expr->type = EXPR_FREE;
	trace(TR_CHECK, "free");
	expr->free.expr = xcalloc(sizeof(struct expression), 1);
	check_expression(ctx, aexpr->free.expr, expr->free.expr, NULL);
	enum type_storage storage = type_dealias(expr->free.expr->result)->storage;
	expect(&aexpr->free.expr->loc,
		storage == STORAGE_SLICE || storage == STORAGE_STRING
		|| storage == STORAGE_POINTER,
		"free must operate on slice, string, or pointer");
	expr->result = &builtin_type_void;
}

static void
check_expr_if(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "if");
	expr->type = EXPR_IF;

	struct expression *cond, *true_branch, *false_branch = NULL;

	cond = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_if.cond, cond, &builtin_type_bool);

	true_branch = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_if.true_branch, true_branch, hint);

	if (aexpr->_if.false_branch) {
		false_branch = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->_if.false_branch,
				false_branch, hint);

		if (true_branch->terminates && false_branch->terminates) {
			expr->terminates = true;
			expr->result = &builtin_type_void;
		} else if (true_branch->terminates) {
			expr->result = false_branch->result;
		} else if (false_branch->terminates) {
			expr->result = true_branch->result;
		} else if (hint && type_is_assignable(hint, true_branch->result)
				&& type_is_assignable(hint, false_branch->result)) {
			expr->result = hint;
		} else if (true_branch->result == false_branch->result) {
			expr->result = true_branch->result;
		} else {
			struct type_tagged_union _tags = {
				.type = false_branch->result,
				.next = NULL,
			}, tags = {
				.type = true_branch->result,
				.next = &_tags,
			};
			expr->result =
				type_store_lookup_tagged(ctx->store, &tags);
		}
		true_branch = lower_implicit_cast(expr->result, true_branch);
		false_branch = lower_implicit_cast(expr->result, false_branch);
	} else {
		expr->result = &builtin_type_void;
		expr->terminates = false;
	}

	expect(&aexpr->_if.cond->loc,
		cond->result->storage == STORAGE_BOOL,
		"Expected if condition to be boolean");

	expr->_if.cond = cond;
	expr->_if.true_branch = true_branch;
	expr->_if.false_branch = false_branch;

	trleave(TR_CHECK, NULL);
}

static void
check_expr_list(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "expression-list");
	expr->type = EXPR_LIST;

	struct scope *scope = scope_push(&ctx->scope, TR_CHECK);
	expr->list.scope = scope;
	scope->type = expr->type;

	struct expressions *list = &expr->list.exprs;
	struct expressions **next = &list->next;

	const struct ast_expression_list *alist = &aexpr->list;
	while (alist) {
		struct expression *lexpr = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, alist->expr, lexpr, NULL);
		list->expr = lexpr;

		alist = alist->next;
		if (alist) {
			*next = xcalloc(1, sizeof(struct expressions));
			list = *next;
			next = &list->next;
		} else {
			expr->result = lexpr->result;
			expr->terminates = lexpr->terminates;
		}
	}

	scope_pop(&ctx->scope, TR_CHECK);
	trleave(TR_CHECK, NULL);
}

static void
check_expr_match(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "match");
	expr->type = EXPR_MATCH;

	struct expression *value = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->match.value, value, NULL);
	expr->match.value = value;

	const struct type *type = type_dealias(value->result);
	bool is_ptr = type->storage == STORAGE_POINTER
		&& type->pointer.flags & PTR_NULLABLE;
	expect(&aexpr->match.value->loc,
		type->storage == STORAGE_TAGGED || is_ptr,
		"match value must be tagged union or nullable pointer type");

	struct type_tagged_union result_type = {0};
	struct type_tagged_union *tagged = &result_type,
		**next_tag = &tagged->next;

	struct match_case **next = &expr->match.cases, *_case = NULL;
	for (struct ast_match_case *acase = aexpr->match.cases;
			acase; acase = acase->next) {
		_case = *next = xcalloc(1, sizeof(struct match_case));
		next = &_case->next;

		const struct type *ctype = NULL;
		if (acase->type) {
			ctype = type_store_lookup_atype(ctx->store, acase->type);
			if (is_ptr) {
				switch (ctype->storage) {
				case STORAGE_NULL:
					break;
				case STORAGE_POINTER:
					expect(&acase->type->loc,
						type->pointer.referent == ctype->pointer.referent,
						"Match case on incompatible pointer type");
					break;
				default:
					expect(&acase->type->loc, false,
						"Invalid type for match case (expected null or pointer type)");
					break;
				}
			} else {
				// TODO: Assign a score to tagged compatibility
				// and choose the branch with the highest score.
				expect(&acase->type->loc, type_is_assignable(type, ctype),
					"Invalid type for match case (match is not assignable to this type)");
			}
		}

		if (acase->name) {
			assert(ctype);
			struct identifier ident = {
				.name = acase->name,
			};
			struct scope *scope = scope_push(&ctx->scope, TR_CHECK);
			scope->type = EXPR_MATCH;
			_case->object = scope_insert(scope, O_BIND,
				&ident, &ident, ctype, NULL);
		}

		_case->value = xcalloc(1, sizeof(struct expression));
		_case->type = ctype;
		check_expression(ctx, acase->value, _case->value, hint);

		if (acase->name) {
			scope_pop(&ctx->scope, TR_CHECK);
		}

		if (_case->value->terminates) {
			continue;
		}

		if (expr->result == NULL) {
			expr->result = _case->value->result;
			tagged->type = expr->result;
		} else if (expr->result != _case->value->result) {
			tagged = *next_tag =
				xcalloc(1, sizeof(struct type_tagged_union));
			next_tag = &tagged->next;
			tagged->type = _case->value->result;
		}
	}

	if (expr->result == NULL) {
		expr->result = &builtin_type_void;
		expr->terminates = true;
	}

	if (result_type.next) {
		if (hint) {
			expr->result = hint;
		} else {
			expr->result = type_store_lookup_tagged(
				ctx->store, &result_type);
		}

		struct match_case *_case = expr->match.cases;
		struct ast_match_case *acase = aexpr->match.cases;
		while (_case) {
			expect(&acase->value->loc,
				_case->value->terminates ||
					type_is_assignable(expr->result, _case->value->result),
				"Match case is not assignable to result type");
			_case->value = lower_implicit_cast(
				expr->result, _case->value);
			_case = _case->next;
			acase = acase->next;
		}

		struct type_tagged_union *tu = result_type.next;
		while (tu) {
			struct type_tagged_union *next = tu->next;
			free(tu);
			tu = next;
		}
	}

	trleave(TR_CHECK, NULL);
}

static void
check_expr_measure(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "measure");
	expr->type = EXPR_MEASURE;
	expr->result = &builtin_type_size;
	expr->measure.op = aexpr->measure.op;

	switch (expr->measure.op) {
	case M_LEN:
		expr->measure.value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->measure.value,
			expr->measure.value, NULL);
		enum type_storage vstor =
			type_dealias(expr->measure.value->result)->storage;
		expect(&aexpr->measure.value->loc,
			vstor == STORAGE_ARRAY
				|| vstor == STORAGE_SLICE
				|| vstor == STORAGE_STRING,
			"len argument must be of an array, slice, or str type");
		expect(&aexpr->measure.value->loc,
			expr->measure.value->result->size != SIZE_UNDEFINED,
			"Cannot take length of array type with undefined length");
		break;
	case M_SIZE:
		expr->measure.type = type_store_lookup_atype(
			ctx->store, aexpr->measure.type);
		break;
	case M_OFFSET:
		assert(0); // TODO
	}
}

static void
check_expr_propagate(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "propagate");
	struct expression *lvalue = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->propagate.value, lvalue, hint);

	const struct type *intype = lvalue->result;
	expect(&aexpr->loc,
		type_dealias(intype)->storage,
		"Cannot use error propagation with non-tagged type");

	struct type_tagged_union result_tagged = {0};
	struct type_tagged_union *tagged = &result_tagged,
		**next_tag = &tagged->next;

	struct type_tagged_union return_tagged = {0};
	struct type_tagged_union *rtagged = &return_tagged,
		**next_rtag = &rtagged->next;

	const struct type_tagged_union *intu = &type_dealias(intype)->tagged;
	for (; intu; intu = intu->next) {
		if (intu->type->flags & TYPE_ERROR) {
			if (rtagged->type) {
				rtagged = *next_rtag =
					xcalloc(1, sizeof(struct type_tagged_union));
				next_rtag = &rtagged->next;
				rtagged->type = intu->type;
			} else {
				rtagged->type = intu->type;
			}
		} else {
			if (tagged->type) {
				tagged = *next_tag =
					xcalloc(1, sizeof(struct type_tagged_union));
				next_tag = &tagged->next;
				tagged->type = intu->type;
			} else {
				tagged->type = intu->type;
			}
		}
	}

	expect(&aexpr->loc, return_tagged.type,
		"No error can occur here, cannot propagate");

	const struct type *return_type;
	if (return_tagged.next) {
		return_type = type_store_lookup_tagged(
			ctx->store, &return_tagged);
	} else {
		return_type = return_tagged.type;
	}

	const struct type *result_type;
	if (!result_tagged.type) {
		result_type = &builtin_type_void;
	} else if (result_tagged.next) {
		result_type = type_store_lookup_tagged(
			ctx->store, &result_tagged);
	} else {
		result_type = result_tagged.type;
	}

	expect(&aexpr->loc,
		type_is_assignable(ctx->fntype->func.result, return_type),
		"Error type is not assignable to function result type");

	// Lower to a match expression
	expr->type = EXPR_MATCH;
	expr->match.value = lvalue;

	struct scope *scope = scope_push(&ctx->scope, TR_CHECK);
	scope->type = EXPR_MATCH;
	struct match_case *case_ok = xcalloc(1, sizeof(struct match_case));
	struct match_case *case_err = xcalloc(1, sizeof(struct match_case));
	struct identifier ok_name = {0}, err_name = {0};

	int n = snprintf(NULL, 0, "ok.%d", ctx->id);
	ok_name.name = xcalloc(n + 1, 1);
	snprintf(ok_name.name, n + 1, "ok.%d", ctx->id);
	++ctx->id;
	const struct scope_object *ok_obj = scope_insert(scope, O_BIND,
			&ok_name, &ok_name, result_type, NULL);

	n = snprintf(NULL, 0, "err.%d", ctx->id);
	err_name.name = xcalloc(n + 1, 1);
	snprintf(err_name.name, n + 1, "err.%d", ctx->id);
	++ctx->id;
	const struct scope_object *err_obj = scope_insert(scope, O_BIND,
			&err_name, &err_name, return_type, NULL);

	case_ok->type = result_type;
	case_ok->object = ok_obj;
	case_ok->value = xcalloc(1, sizeof(struct expression));
	case_ok->value->type = EXPR_ACCESS;
	case_ok->value->access.type = ACCESS_IDENTIFIER;
	case_ok->value->access.object = ok_obj;
	case_ok->value->result = result_type;

	case_err->type = return_type;
	case_err->object = err_obj;
	case_err->value = xcalloc(1, sizeof(struct expression));
	case_err->value->type = EXPR_RETURN;
	case_err->value->terminates = true;
	case_err->value->result = &builtin_type_void;
	struct expression *rval = 
		xcalloc(1, sizeof(struct expression));
	rval->type = EXPR_ACCESS;
	rval->access.type = ACCESS_IDENTIFIER;
	rval->access.object = err_obj;
	rval->result = return_type;
	case_err->value->_return.value = lower_implicit_cast(
			ctx->fntype->func.result, rval);

	expr->match.cases = case_ok;
	case_ok->next = case_err;

	scope_pop(&ctx->scope, TR_CHECK);
	expr->result = result_type;
}

static void
check_expr_return(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "return");
	expr->type = EXPR_RETURN;
	expr->result = &builtin_type_void;
	expr->terminates = true;

	struct expression *rval = xcalloc(1, sizeof(struct expression));
	if (aexpr->_return.value) {
		check_expression(ctx, aexpr->_return.value,
			rval, ctx->fntype->func.result);
	} else {
		rval->type = EXPR_CONSTANT;
		rval->result = &builtin_type_void;
	}

	expect(&aexpr->_return.value->loc,
		type_is_assignable(ctx->fntype->func.result, rval->result),
		"Return value is not assignable to function result type");
	if (ctx->fntype->func.result != rval->result) {
		rval = lower_implicit_cast(
			ctx->fntype->func.result, rval);
	}
	expr->_return.value = rval;

	trleave(TR_CHECK, NULL);
}

static void
check_expr_slice(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "slice");
	expr->type = EXPR_SLICE;

	expr->slice.object = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->slice.object, expr->slice.object, NULL);
	const struct type *atype =
		type_dereference(expr->slice.object->result);
	expect(&aexpr->slice.object->loc, atype,
		"Cannot dereference nullable pointer for slicing");
	expect(&aexpr->slice.object->loc,
		atype->storage == STORAGE_SLICE
			|| atype->storage == STORAGE_ARRAY,
		"Cannot slice non-array, non-slice object");

	const struct type *itype;
	if (aexpr->slice.start) {
		expr->slice.start = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->slice.start, expr->slice.start, NULL);
		itype = type_dealias(expr->slice.start->result);
		expect(&aexpr->slice.start->loc, type_is_integer(itype),
			"Cannot use non-integer %s type as slicing operand",
			type_storage_unparse(itype->storage));
		expr->slice.start = lower_implicit_cast(
			&builtin_type_size, expr->slice.start);
	}

	if (aexpr->slice.end) {
		expr->slice.end = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->slice.end, expr->slice.end, NULL);
		itype = type_dealias(expr->slice.end->result);
		expect(&aexpr->slice.end->loc, type_is_integer(itype),
			"Cannot use non-integer %s type as slicing operand",
			type_storage_unparse(itype->storage));
		expr->slice.end = lower_implicit_cast(
			&builtin_type_size, expr->slice.end);
	} else {
		// TODO: Assert that array type has a well-defined length
	}

	expr->result = type_store_lookup_slice(ctx->store, atype->array.members);

	trleave(TR_CHECK, NULL);
}

static void
check_expr_struct(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "struct");
	expr->type = EXPR_STRUCT;

	const struct type *stype = NULL;
	if (aexpr->_struct.type.name) {
		const struct scope_object *obj = scope_lookup(ctx->scope,
				&aexpr->_struct.type);
		expect(&aexpr->loc, obj, "Unknown type alias");
		expect(&aexpr->loc, obj->otype == O_TYPE,
				"Name does not refer to a type");
		stype = obj->type;
		expect(&aexpr->loc,
			type_dealias(stype)->storage == STORAGE_STRUCT,
			"Object named is not a struct type");
	}

	struct ast_type satype = {
		.storage = STORAGE_STRUCT,
		.flags = TYPE_CONST,
	};
	struct ast_struct_union_type *tfield = &satype.struct_union;
	struct ast_struct_union_type **tnext = &tfield->next;
	struct expr_struct_field *sexpr = &expr->_struct.fields;
	struct expr_struct_field **snext = &sexpr->next;
	expr->_struct.autofill = aexpr->_struct.autofill;
	expect(&aexpr->loc, stype != NULL || !expr->_struct.autofill,
			"Autofill is only permitted for named struct initializers");

	struct ast_field_value *afield = aexpr->_struct.fields;
	while (afield) {
		assert(!afield->is_embedded); // TODO

		const struct type *ftype;
		if (!stype) {
			tfield->member_type = MEMBER_TYPE_FIELD;
			tfield->field.name = afield->field.name;
			tfield->field.type = afield->field.type;
			ftype = type_store_lookup_atype(
				ctx->store, tfield->field.type);
		} else {
			expect(&afield->field.initializer->loc, afield->field.name,
				"Anonymous fields are not permitted for named struct type");
			// XXX: ^ Is that correct?
			sexpr->field = type_get_field(type_dealias(stype),
					afield->field.name);
			expect(&afield->field.initializer->loc, sexpr->field,
				"No field by this name exists for this type");
			ftype = sexpr->field->type;
		}

		sexpr->value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, afield->field.initializer,
				sexpr->value, ftype);

		if (stype) {
			expect(&afield->field.initializer->loc,
				type_is_assignable(sexpr->field->type, sexpr->value->result),
				"Initializer is not assignable to struct field");
			sexpr->value = lower_implicit_cast(
				sexpr->field->type, sexpr->value);
		}

		if (afield->next) {
			if (!stype) {
				*tnext = tfield = xcalloc(
					1, sizeof(struct ast_struct_union_type));
				tnext = &tfield->next;
			}
			*snext = sexpr = xcalloc(1, sizeof(struct expr_struct_field));
			snext = &sexpr->next;
		}

		afield = afield->next;
	}

	if (stype) {
		// TODO: Test for exhaustiveness
		expr->result = stype;
	} else {
		expr->result = type_store_lookup_atype(ctx->store, &satype);

		tfield = &satype.struct_union;
		sexpr = &expr->_struct.fields;
		while (tfield) {
			const struct struct_field *field = type_get_field(
				expr->result, tfield->field.name);
			// TODO: Use more specific error location
			expect(&aexpr->loc, field,
				"No field by this name exists for this type");
			expect(&aexpr->loc,
				type_is_assignable(field->type, sexpr->value->result),
				"Cannot initialize struct field '%s' from value of this type",
				field->name);
			sexpr->field = field;
			sexpr->value = lower_implicit_cast(field->type, sexpr->value);

			struct ast_struct_union_type *next = tfield->next;
			if (tfield != &satype.struct_union) {
				free(tfield);
			}
			tfield = next;
			sexpr = sexpr->next;
		}
	}

	trleave(TR_CHECK, NULL);
}

static void
check_expr_switch(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "switch");
	expr->type = EXPR_SWITCH;

	struct expression *value = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_switch.value, value, NULL);
	const struct type *type = type_dealias(value->result);
	expr->_switch.value = value;

	struct type_tagged_union result_type = {0};
	struct type_tagged_union *tagged = &result_type,
		**next_tag = &tagged->next;

	// TODO: Test for dupes, exhaustiveness
	struct switch_case **next = &expr->_switch.cases, *_case = NULL;
	for (struct ast_switch_case *acase = aexpr->_switch.cases;
			acase; acase = acase->next) {
		_case = *next = xcalloc(1, sizeof(struct switch_case));
		next = &_case->next;

		struct case_option *opt, **next_opt = &_case->options;
		for (struct ast_case_option *aopt = acase->options;
				aopt; aopt = aopt->next) {
			opt = *next_opt = xcalloc(1, sizeof(struct case_option));
			struct expression *value =
				xcalloc(1, sizeof(struct expression));
			struct expression *evaled =
				xcalloc(1, sizeof(struct expression));

			check_expression(ctx, aopt->value, value, type);
			expect(&aopt->value->loc,
				type_is_assignable(
					type_dealias(type),
					type_dealias(value->result)),
				"Invalid type for switch case");

			enum eval_result r = eval_expr(ctx, value, evaled);
			expect(&aopt->value->loc,
				r == EVAL_OK,
				"Unable to evaluate case at compile time");

			opt->value = evaled;
			next_opt = &opt->next;
		}

		_case->value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, acase->value, _case->value, hint);
		if (_case->value->terminates) {
			continue;
		}

		if (expr->result == NULL) {
			expr->result = _case->value->result;
			tagged->type = expr->result;
		} else if (expr->result != _case->value->result) {
			tagged = *next_tag =
				xcalloc(1, sizeof(struct type_tagged_union));
			next_tag = &tagged->next;
			tagged->type = _case->value->result;
		}
	}

	if (expr->result == NULL) {
		expr->result = &builtin_type_void;
		expr->terminates = true;
	}

	if (result_type.next) {
		if (hint) {
			expr->result = hint;
		} else {
			expr->result = type_store_lookup_tagged(
				ctx->store, &result_type);
		}

		struct switch_case *_case = expr->_switch.cases;
		struct ast_switch_case *acase = aexpr->_switch.cases;
		while (_case) {
			expect(&acase->value->loc,
				_case->value->terminates ||
					type_is_assignable(expr->result, _case->value->result),
				"Switch case is not assignable to result type");
			_case->value = lower_implicit_cast(
				expr->result, _case->value);
			_case = _case->next;
			acase = acase->next;
		}

		struct type_tagged_union *tu = result_type.next;
		while (tu) {
			struct type_tagged_union *next = tu->next;
			free(tu);
			tu = next;
		}
	}
}

static void
check_expr_tuple(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "tuple");
	expr->type = EXPR_TUPLE;

	const struct type_tuple *ttuple = NULL;
	if (hint && type_dealias(hint)->storage == STORAGE_TUPLE) {
		ttuple = &type_dealias(hint)->tuple;
	}

	struct type_tuple result = {0};
	struct type_tuple *rtype = &result;

	struct expression_tuple *tuple = &expr->tuple;
	for (const struct ast_expression_tuple *atuple = &aexpr->tuple;
			atuple; atuple = atuple->next) {
		tuple->value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, atuple->expr, tuple->value,
				ttuple ? ttuple->type : NULL);
		rtype->type = tuple->value->result;

		if (atuple->next) {
			rtype->next = xcalloc(1, sizeof(struct type_tuple));
			rtype = rtype->next;
			tuple->next = xcalloc(1, sizeof(struct expression_tuple));
			tuple = tuple->next;
		}

		if (ttuple) {
			ttuple = ttuple->next;
		}
	}

	if (hint && type_dealias(hint)->storage == STORAGE_TUPLE) {
		expr->result = hint;
	} else {
		expr->result = type_store_lookup_tuple(ctx->store, &result);
	}

	ttuple = &type_dealias(expr->result)->tuple;
	struct expression_tuple *etuple = &expr->tuple;
	const struct ast_expression_tuple *atuple = &aexpr->tuple;
	while (etuple) {
		expect(&atuple->expr->loc, ttuple,
			"Too many values for tuple type");
		expect(&atuple->expr->loc,
			type_is_assignable(ttuple->type, etuple->value->result),
			"Value is not assignable to tuple value type");
		etuple->value = lower_implicit_cast(ttuple->type, etuple->value);
		etuple = etuple->next;
		atuple = atuple->next;
		ttuple = ttuple->next;
	}

	trleave(TR_CHECK, NULL);
}

static void
check_expr_unarithm(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "unarithm");
	expr->type = EXPR_UNARITHM;

	struct expression *operand = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->unarithm.operand, operand, NULL);
	expr->unarithm.operand = operand;
	expr->unarithm.op = aexpr->unarithm.op;

	switch (expr->unarithm.op) {
	case UN_LNOT:
		expect(&aexpr->unarithm.operand->loc,
			operand->result->storage == STORAGE_BOOL,
			"Cannot perform logical NOT (!) on non-boolean type");
		expr->result = &builtin_type_bool;
		break;
	case UN_BNOT:
		expect(&aexpr->unarithm.operand->loc,
			type_is_integer(operand->result),
			"Cannot perform binary NOT (~) on non-integer type");
		expect(&aexpr->unarithm.operand->loc,
			!type_is_signed(operand->result),
			"Cannot perform binary NOT (~) on signed type");
		expr->result = operand->result;
		break;
	case UN_MINUS:
	case UN_PLUS:
		expect(&aexpr->unarithm.operand->loc,
			type_is_numeric(operand->result),
			"Cannot perform operation on non-numeric type");
		expect(&aexpr->unarithm.operand->loc,
			type_is_signed(operand->result),
			"Cannot perform operation on unsigned type");
		expr->result = operand->result;
		break;
	case UN_ADDRESS:
		expr->result = type_store_lookup_pointer(
			ctx->store, operand->result, 0);
		break;
	case UN_DEREF:
		expect(&aexpr->unarithm.operand->loc,
			operand->result->storage == STORAGE_POINTER,
			"Cannot de-reference non-pointer type");
		expect(&aexpr->unarithm.operand->loc,
			!(operand->result->pointer.flags & PTR_NULLABLE),
			"Cannot dereference nullable pointer type");
		expr->result = operand->result->pointer.referent;
		break;
	}

	trleave(TR_CHECK, NULL);
}

void
check_expression(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	trenter(TR_CHECK, "expression");

	switch (aexpr->type) {
	case EXPR_ACCESS:
		check_expr_access(ctx, aexpr, expr, hint);
		break;
	case EXPR_ALLOC:
		check_expr_alloc(ctx, aexpr, expr, hint);
		break;
	case EXPR_APPEND:
		check_expr_append(ctx, aexpr, expr, hint);
		break;
	case EXPR_ASSERT:
		check_expr_assert(ctx, aexpr, expr, hint);
		break;
	case EXPR_ASSIGN:
		check_expr_assign(ctx, aexpr, expr, hint);
		break;
	case EXPR_BINARITHM:
		check_expr_binarithm(ctx, aexpr, expr, hint);
		break;
	case EXPR_BINDING:
		check_expr_binding(ctx, aexpr, expr, hint);
		break;
	case EXPR_BREAK:
	case EXPR_CONTINUE:
		check_expr_control(ctx, aexpr, expr, hint);
		break;
	case EXPR_CALL:
		check_expr_call(ctx, aexpr, expr, hint);
		break;
	case EXPR_CAST:
		check_expr_cast(ctx, aexpr, expr, hint);
		break;
	case EXPR_CONSTANT:
		check_expr_constant(ctx, aexpr, expr, hint);
		break;
	case EXPR_DEFER:
		check_expr_defer(ctx, aexpr, expr, hint);
		break;
	case EXPR_DELETE:
		check_expr_delete(ctx, aexpr, expr, hint);
		break;
	case EXPR_FOR:
		check_expr_for(ctx, aexpr, expr, hint);
		break;
	case EXPR_FREE:
		check_expr_free(ctx, aexpr, expr, hint);
		break;
	case EXPR_IF:
		check_expr_if(ctx, aexpr, expr, hint);
		break;
	case EXPR_LIST:
		check_expr_list(ctx, aexpr, expr, hint);
		break;
	case EXPR_MATCH:
		check_expr_match(ctx, aexpr, expr, hint);
		break;
	case EXPR_MEASURE:
		check_expr_measure(ctx, aexpr, expr, hint);
		break;
	case EXPR_PROPAGATE:
		check_expr_propagate(ctx, aexpr, expr, hint);
		break;
	case EXPR_RETURN:
		check_expr_return(ctx, aexpr, expr, hint);
		break;
	case EXPR_SLICE:
		check_expr_slice(ctx, aexpr, expr, hint);
		break;
	case EXPR_STRUCT:
		check_expr_struct(ctx, aexpr, expr, hint);
		break;
	case EXPR_SWITCH:
		check_expr_switch(ctx, aexpr, expr, hint);
		break;
	case EXPR_TUPLE:
		check_expr_tuple(ctx, aexpr, expr, hint);
		break;
	case EXPR_UNARITHM:
		check_expr_unarithm(ctx, aexpr, expr, hint);
		break;
	}

	trleave(TR_CHECK, NULL);
	assert(expr->result);
}

static struct declaration *
check_const(struct context *ctx,
	const struct ast_decl *adecl)
{
	const struct type *type = type_store_lookup_atype(
			ctx->store, adecl->constant.type);
	struct declaration *decl = xcalloc(1, sizeof(struct declaration));
	const struct scope_object *obj = scope_lookup(
			ctx->unit, &adecl->constant.ident);
	assert(obj && obj->otype == O_CONST);
	decl->type = DECL_CONST;
	decl->constant.type = type;
	decl->constant.value = obj->value;
	mkident(ctx, &decl->ident, &adecl->constant.ident);
	return decl;
}

static struct declaration *
check_function(struct context *ctx,
	const struct ast_decl *adecl)
{
	if (!adecl->function.body) {
		return NULL; // Prototype
	}

	const struct ast_function_decl *afndecl = &adecl->function;
	if ((adecl->function.flags & FN_TEST) && !tag_enabled(ctx->tags, "test")) {
		return NULL;
	}

	trenter(TR_CHECK, "function");
	const struct ast_type fn_atype = {
		.storage = STORAGE_FUNCTION,
		.flags = TYPE_CONST,
		.func = afndecl->prototype,
	};
	const struct type *fntype = type_store_lookup_atype(
			ctx->store, &fn_atype);
	assert(fntype); // Invariant
	ctx->fntype = fntype;

	expect(&adecl->loc,
		fntype->func.variadism != VARIADISM_C,
		"C-style variadism is not allowed for function declarations");

	struct declaration *decl = xcalloc(1, sizeof(struct declaration));
	decl->type = DECL_FUNC;
	decl->func.type = fntype;
	decl->func.flags = afndecl->flags;

	if (afndecl->symbol) {
		decl->symbol = strdup(afndecl->symbol);
	}
	mkident(ctx, &decl->ident, &afndecl->ident);

	decl->func.scope = scope_push(&ctx->scope, TR_CHECK);
	struct ast_function_parameters *params = afndecl->prototype.params;
	while (params) {
		expect(&params->loc, params->name,
			"Function parameters must be named");
		struct identifier ident = {
			.name = params->name,
		};
		const struct type *type = type_store_lookup_atype(
				ctx->store, params->type);
		if (fntype->func.variadism == VARIADISM_HARE
				&& !params->next) {
			type = type_store_lookup_slice(ctx->store, type);
		}
		scope_insert(decl->func.scope, O_BIND,
			&ident, &ident, type, NULL);
		params = params->next;
	}

	struct expression *body = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, afndecl->body, body, fntype->func.result);

	expect(&afndecl->body->loc,
		body->terminates || type_is_assignable(fntype->func.result, body->result),
		"Result value is not assignable to function result type");
	if (!body->terminates && fntype->func.result != body->result) {
		body = lower_implicit_cast(fntype->func.result, body);
	}
	decl->func.body = body;

	// TODO: Add function name to errors
	if ((decl->func.flags & FN_INIT)
			|| (decl->func.flags & FN_FINI)
			|| (decl->func.flags & FN_TEST)) {
		const char *flags = "@flags"; // TODO: Unparse flags
		expect(&adecl->loc, fntype->func.result == &builtin_type_void,
				"%s function must return void", flags);
		expect(&adecl->loc, !decl->exported,
				"%s function cannot be exported", flags);
	}

	scope_pop(&ctx->scope, TR_CHECK);
	ctx->fntype = NULL;
	trleave(TR_CHECK, NULL);
	return decl;
}

static struct declaration *
check_global(struct context *ctx,
	const struct ast_decl *adecl)
{
	const struct ast_global_decl *agdecl = &adecl->global;
	if (!agdecl->init) {
		return NULL; // Forward declaration
	}

	const struct type *type = type_store_lookup_atype(
			ctx->store, agdecl->type);
	bool context = agdecl->type->storage == STORAGE_ARRAY
			&& agdecl->type->array.contextual;

	// TODO: Free initialier
	struct expression *initializer =
		xcalloc(1, sizeof(struct expression));
	check_expression(ctx, agdecl->init, initializer, type);

	if (context) {
		expect(&agdecl->init->loc,
			initializer->result->storage == STORAGE_ARRAY,
			"Cannot infer array length from non-array type");
		expect(&agdecl->init->loc,
			initializer->result->array.members == type->array.members,
			"Initializer is not assignable to binding type");
		type = initializer->result;

		// Update type of object (drops const, hack!)
		struct scope_object *obj = (struct scope_object *)scope_lookup(
				ctx->scope, &agdecl->ident);
		obj->type = type;
	}

	expect(&agdecl->init->loc,
		type_is_assignable(type, initializer->result),
		"Constant type is not assignable from initializer type");
	initializer = lower_implicit_cast(type, initializer);

	struct expression *value =
		xcalloc(1, sizeof(struct expression));
	enum eval_result r = eval_expr(ctx, initializer, value);
	expect(&agdecl->init->loc, r == EVAL_OK,
		"Unable to evaluate global initializer at compile time");

	struct declaration *decl = xcalloc(1, sizeof(struct declaration));
	decl->type = DECL_GLOBAL;
	decl->global.type = type;
	decl->global.value = value;

	if (agdecl->symbol) {
		decl->ident.name = strdup(agdecl->symbol);
		decl->symbol = strdup(agdecl->symbol);
	} else {
		mkident(ctx, &decl->ident, &agdecl->ident);
	}

	return decl;
}

static struct declaration *
check_type(struct context *ctx,
	const struct ast_decl *adecl)
{
	const struct type *type =
		type_store_lookup_atype(ctx->store, adecl->type.type);
	struct declaration *decl = xcalloc(1, sizeof(struct declaration));
	decl->type = DECL_TYPE;
	decl->_type = type;
	mkident(ctx, &decl->ident, &adecl->type.ident);
	return decl;
}

static struct declarations **
check_declarations(struct context *ctx,
		const struct ast_decls *adecls,
		struct declarations **next)
{
	trenter(TR_CHECK, "declarations");
	while (adecls) {
		struct declaration *decl = NULL;
		const struct ast_decl *adecl = &adecls->decl;
		switch (adecl->decl_type) {
		case AST_DECL_CONST:
			decl = check_const(ctx, adecl);
			break;
		case AST_DECL_FUNC:
			decl = check_function(ctx, adecl);
			break;
		case AST_DECL_GLOBAL:
			decl = check_global(ctx, adecl);
			break;
		case AST_DECL_TYPE:
			decl = check_type(ctx, adecl);
			break;
		}

		if (decl) {
			struct declarations *decls = *next =
				xcalloc(1, sizeof(struct declarations));
			decl->exported = adecl->exported;
			decls->decl = decl;
			next = &decls->next;
		}

		adecls = adecls->next;
	}
	trleave(TR_CHECK, NULL);
	return next;
}

static void
scan_const(struct context *ctx, const struct ast_global_decl *decl)
{
	trenter(TR_SCAN, "constant");
	assert(!decl->symbol); // Invariant

	const struct type *type = type_store_lookup_atype(
			ctx->store, decl->type);
	// TODO:
	// - Free the initializer
	// - Defer if we can't evaluate it now (for forward references)
	struct expression *initializer =
		xcalloc(1, sizeof(struct expression));
	check_expression(ctx, decl->init, initializer, type);

	expect(&decl->init->loc, type_is_assignable(type, initializer->result),
		"Constant type is not assignable from initializer type");
	initializer = lower_implicit_cast(type, initializer);

	struct expression *value =
		xcalloc(1, sizeof(struct expression));
	enum eval_result r = eval_expr(ctx, initializer, value);
	// TODO: More forward reference issues:
	expect(&decl->init->loc, r == EVAL_OK,
		"Unable to evaluate constant initializer at compile time");

	struct identifier ident = {0};
	mkident(ctx, &ident, &decl->ident);
	scope_insert(ctx->unit, O_CONST, &ident, &decl->ident, type, value);

	trleave(TR_SCAN, NULL);
}

static void
scan_function(struct context *ctx, const struct ast_function_decl *decl)
{
	if ((decl->flags & FN_TEST) && !tag_enabled(ctx->tags, "test")) {
		return;
	}
	trenter(TR_SCAN, "function");
	const struct ast_type fn_atype = {
		.storage = STORAGE_FUNCTION,
		.flags = TYPE_CONST,
		.func = decl->prototype,
	};
	const struct type *fntype = type_store_lookup_atype(
			ctx->store, &fn_atype);
	assert(fntype); // TODO: Forward references

	if (!(decl->flags & FN_TEST)) {
		struct identifier ident = {0};
		if (decl->symbol) {
			ident.name = strdup(decl->symbol);
		} else if (!decl->ident.ns) {
			mkident(ctx, &ident, &decl->ident);
		} else {
			ident = decl->ident;
		}
		scope_insert(ctx->unit, O_DECL, &ident, &decl->ident, fntype, NULL);
	}

	char buf[1024];
	identifier_unparse_static(&decl->ident, buf, sizeof(buf));
	trleave(TR_SCAN, "func %s", buf);
}

static void
scan_global(struct context *ctx, const struct ast_global_decl *decl)
{
	trenter(TR_SCAN, "global");

	const struct type *type = type_store_lookup_atype(
			ctx->store, decl->type);
	assert(type); // TODO: Forward references

	struct identifier ident = {0};
	if (decl->symbol) {
		ident.name = strdup(decl->symbol);
	} else {
		mkident(ctx, &ident, &decl->ident);
	}
	scope_insert(ctx->unit, O_DECL, &ident, &decl->ident, type, NULL);

	trleave(TR_SCAN, NULL);
}

static void
scan_type(struct context *ctx, const struct ast_type_decl *decl)
{
	trenter(TR_SCAN, "type");
	const struct type *type =
		type_store_lookup_atype(ctx->store, decl->type);

	struct identifier ident = {0};
	mkident(ctx, &ident, &decl->ident);

	const struct type *alias =
		type_store_lookup_alias(ctx->store, &ident, type);
	scope_insert(ctx->unit, O_TYPE, &ident, &decl->ident, alias, NULL);

	if (type->storage == STORAGE_ENUM) {
		for (struct type_enum_value *value = type->_enum.values; value;
				value = value->next) {
			struct ast_type atype = {
				.loc = decl->type->loc,
				.storage = STORAGE_ALIAS,
				.flags = 0,
				.unwrap = false,
				.alias = decl->ident,
			};
			const struct type *alias =
				type_store_lookup_atype(ctx->store, &atype);

			struct expression *expr =
				xcalloc(sizeof(struct expression), 1);
			expr->type = EXPR_CONSTANT;
			expr->result = alias;
			if (type_is_signed(alias)) {
				expr->constant.ival = value->ival;
			} else {
				expr->constant.uval = value->uval;
			}

			struct identifier name_ns = {
				.name = decl->ident.name,
				.ns = decl->ident.ns,
			};
			struct identifier name = {
				.name = value->name,
				.ns = &name_ns,
			};
			struct identifier vident = {
				.name = value->name,
				.ns = &ident,
			};
			scope_insert(ctx->unit, O_CONST, &name, &vident, alias, expr);
		}
	}
	trleave(TR_SCAN, NULL);
}

static void
scan_declarations(struct context *ctx, const struct ast_decls *decls)
{
	trenter(TR_SCAN, "declarations");
	while (decls) {
		const struct ast_decl *decl = &decls->decl;
		switch (decl->decl_type) {
		case AST_DECL_CONST:
			scan_const(ctx, &decl->constant);
			break;
		case AST_DECL_FUNC:
			scan_function(ctx, &decl->function);
			break;
		case AST_DECL_GLOBAL:
			scan_global(ctx, &decl->global);
			break;
		case AST_DECL_TYPE:
			scan_type(ctx, &decl->type);
			break;
		}
		decls = decls->next;
	}
	trleave(TR_SCAN, NULL);
}

static void
load_import(struct ast_imports *import,
	struct type_store *ts, struct scope *scope)
{
	struct scope *mod = module_resolve(&import->ident, ts);

	switch (import->mode) {
	case AST_IMPORT_IDENTIFIER:
		for (struct scope_object *obj = mod->objects;
				obj; obj = obj->next) {
			scope_insert(scope, obj->otype, &obj->ident,
				&obj->name, obj->type, obj->value);
			if (obj->ident.ns && obj->ident.ns->ns) {
				struct identifier ns = {
					.name = obj->ident.ns->name,
					.ns = NULL
				};
				struct identifier name = {
					.name = obj->ident.name,
					.ns = &ns,
				};
				scope_insert(scope, obj->otype, &obj->ident,
					&name, obj->type, obj->value);
			}
		}
		break;
	case AST_IMPORT_ALIAS:
		assert(0); // TODO
	case AST_IMPORT_MEMBERS:
		assert(0); // TODO
	}
}

struct scope *
check(struct type_store *ts, struct build_tags *tags,
		const struct ast_unit *aunit, struct unit *unit)
{
	struct context ctx = {0};
	ctx.ns = unit->ns;
	ctx.tags = tags;
	ctx.store = ts;
	ctx.store->check_context = &ctx;

	// Top-level scope management involves:
	//
	// - Creating a top-level scope for the whole unit, to which
	//   declarations are added.
	// - Creating a scope for each sub-unit, and populating it with imports.
	// 
	// Further down the call frame, subsequent functions will create
	// sub-scopes for each declaration, expression-list, etc.
	ctx.unit = scope_push(&ctx.scope, TR_MAX);

	struct scopes *subunit_scopes;
	struct scopes **next = &subunit_scopes;
	struct imports **inext = &unit->imports;

	// First pass populates the type graph
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		scope_push(&ctx.scope, TR_SCAN);

		for (struct ast_imports *imports = su->imports;
				imports; imports = imports->next) {
			load_import(imports, ts, ctx.scope);

			bool found = false;
			for (struct imports *uimports = unit->imports;
					uimports; uimports = uimports->next) {
				if (identifier_eq(&uimports->ident, &imports->ident)) {
					found = true;
					break;
				}
			}
			if (!found) {
				struct imports *uimport = *inext =
					xcalloc(1, sizeof(struct imports));
				identifier_dup(&uimport->ident, &imports->ident);
				inext = &uimport->next;
			}
		}

		ctx.store->check_context = &ctx;
		scan_declarations(&ctx, &su->decls);

		*next = xcalloc(1, sizeof(struct scopes));
		(*next)->scope = scope_pop(&ctx.scope, TR_SCAN);
		next = &(*next)->next;
	}

	// Second pass populates the expression graph
	struct scopes *scope = subunit_scopes;
	struct declarations **next_decl = &unit->declarations;
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		ctx.scope = scope->scope;
		trenter(TR_CHECK, "scope %p", ctx.scope);
		next_decl = check_declarations(&ctx, &su->decls, next_decl);
		trleave(TR_CHECK, NULL);
		scope = scope->next;
	}

	return ctx.unit;
}
