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
#include "type_store.h"
#include "typedef.h"
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
		exit(EXIT_FAILURE);
	}
}

static char *
gen_typename(const struct type *type)
{
	size_t sz = 0;
	char *ptr = NULL;
	FILE *f = open_memstream(&ptr, &sz);
	emit_type(type, f);
	fclose(f);
	return ptr;
}

static void
handle_errors(struct errors *errors)
{
	struct errors *error = errors;
	while (error) {
		fprintf(stderr, "Error %s:%d:%d: %s\n", error->loc.path,
			error->loc.lineno, error->loc.colno, error->msg);
		struct errors *next = error->next;
		free(error);
		error = next;
	}
	if (errors) {
		exit(EXIT_FAILURE);
	}
}

static void
error(struct context *ctx, const struct location loc, struct expression *expr,
		char *fmt, ...)
{
	expr->type = EXPR_CONSTANT;
	// TODO: We should have a separate type for errors, to avoid spurious
	// errors
	expr->result = &builtin_type_void;
	expr->terminates = false;
	expr->loc = loc;

	va_list ap;
	va_start(ap, fmt);
	size_t sz = vsnprintf(NULL, 0, fmt, ap);
	va_end(ap);
	char *msg = xcalloc(1, sz + 1);
	va_start(ap, fmt);
	vsnprintf(msg, sz + 1, fmt, ap);
	va_end(ap);

	struct errors *next = *ctx->next = xcalloc(1, sizeof(struct errors));
	next->loc = loc;
	next->msg = msg;
	ctx->next = &next->next;
}

static struct expression *
lower_implicit_cast(const struct type *to, struct expression *expr)
{
	if (to == expr->result || expr->terminates) {
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
	cast->terminates = false;
	cast->cast.kind = C_CAST;
	cast->cast.value = expr;
	cast->cast.lowered = true;
	return cast;
}

static void
check_expr_access(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_ACCESS;
	expr->access.type = aexpr->access.type;

	const struct scope_object *obj;
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		obj = scope_lookup(ctx->scope, &aexpr->access.ident);
		char buf[1024];
		identifier_unparse_static(&aexpr->access.ident, buf, sizeof(buf));
		if (!obj) {
			error(ctx, aexpr->loc, expr,
				"Unknown object '%s'", buf);
			return;
		}
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
			if (type_dealias(obj->type)->storage != STORAGE_VOID) {
				error(ctx, aexpr->loc, expr,
					"Cannot use non-void type alias '%s' as constant",
					identifier_unparse(&obj->type->alias.ident));
				return;
			}
			expr->type = EXPR_CONSTANT;
			expr->result = obj->type;
			break;
		}
		break;
	case ACCESS_INDEX:
		expr->access.array = xcalloc(1, sizeof(struct expression));
		expr->access.index = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->access.array, expr->access.array, NULL);
		check_expression(ctx, aexpr->access.index, expr->access.index, &builtin_type_size);
		const struct type *atype =
			type_dereference(expr->access.array->result);
		if (!atype) {
			error(ctx, aexpr->access.array->loc, expr,
				"Cannot dereference nullable pointer for indexing");
			return;
		}
		atype = type_dealias(atype);
		const struct type *itype =
			type_dealias(expr->access.index->result);
		if (atype->storage != STORAGE_ARRAY
				&& atype->storage != STORAGE_SLICE) {
			error(ctx, aexpr->access.array->loc, expr,
				"Cannot index non-array, non-slice %s object",
				type_storage_unparse(atype->storage));
			return;
		}
		if (!type_is_integer(itype)) {
			error(ctx, aexpr->access.index->loc, expr,
				"Cannot use non-integer %s type as slice/array index",
				type_storage_unparse(itype->storage));
			return;
		}
		expr->access.index = lower_implicit_cast(
			&builtin_type_size, expr->access.index);
		expr->result = type_store_lookup_with_flags(ctx->store,
			atype->array.members, atype->flags | atype->array.members->flags);
		break;
	case ACCESS_FIELD:
		expr->access._struct = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->access._struct, expr->access._struct, NULL);
		const struct type *stype =
			type_dereference(expr->access._struct->result);
		if (!stype) {
			error(ctx, aexpr->access._struct->loc, expr,
				"Cannot dereference nullable pointer for field selection");
			return;
		}
		stype = type_dealias(stype);
		if (stype->storage != STORAGE_STRUCT
				&& stype->storage != STORAGE_UNION) {
			error(ctx, aexpr->access._struct->loc, expr,
				"Cannot select field from non-struct, non-union object");
			return;
		}
		expr->access.field = type_get_field(stype, aexpr->access.field);
		if (!expr->access.field) {
			error(ctx, aexpr->access._struct->loc, expr,
				"No such struct field '%s'", aexpr->access.field);
			return;
		}
		expr->result = expr->access.field->type;
		break;
	case ACCESS_TUPLE:
		expr->access.tuple = xcalloc(1, sizeof(struct expression));
		struct expression *value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->access.tuple, expr->access.tuple, NULL);
		check_expression(ctx, aexpr->access.value, value, NULL);
		assert(value->type == EXPR_CONSTANT);

		const struct type *ttype =
			type_dereference(expr->access.tuple->result);
		if (!ttype) {
			error(ctx, aexpr->access.tuple->loc, expr,
				"Cannot dereference nullable pointer for value selection");
			return;
		}
		ttype = type_dealias(ttype);
		if (ttype->storage != STORAGE_TUPLE) {
			error(ctx, aexpr->access.tuple->loc, expr,
				"Cannot select value from non-tuple object");
			return;
		}
		if (!type_is_integer(value->result)) {
			error(ctx, aexpr->access.tuple->loc, expr,
				"Cannot use non-integer constant to select tuple value");
			return;
		}

		expr->access.tvalue = type_get_value(ttype,
			aexpr->access.value->constant.uval);
		if (!expr->access.tvalue) {
			error(ctx, aexpr->access.tuple->loc, expr,
				"No such tuple value '%zu'",
				aexpr->access.value->constant.uval);
			return;
		}
		expr->access.tindex = aexpr->access.value->constant.uval;

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
	expr->type = EXPR_ALLOC;
	expr->alloc.expr = xcalloc(sizeof(struct expression), 1);
	const struct type *inittype = NULL;
	if (hint && type_dealias(hint)->storage == STORAGE_POINTER) {
		inittype = type_dealias(hint)->pointer.referent;
	} else if (hint && type_dealias(hint)->storage == STORAGE_SLICE) {
		inittype = hint;
	}
	check_expression(ctx, aexpr->alloc.expr, expr->alloc.expr, inittype);
	inittype = expr->alloc.expr->result;

	int flags = 0;
	if (hint && type_is_assignable(hint, inittype)) {
		if (type_dealias(hint)->storage == STORAGE_SLICE) {
			inittype = hint;
		} else if (type_dealias(hint)->storage == STORAGE_POINTER) {
			inittype = type_dealias(hint)->pointer.referent;
			flags = type_dealias(hint)->pointer.flags;
		}
	}

	switch (type_dealias(inittype)->storage) {
	case STORAGE_SLICE:
		expr->result = inittype;
		break;
	case STORAGE_ARRAY:
		if (aexpr->alloc.cap) {
			expr->result = type_store_lookup_slice(ctx->store,
				type_dealias(inittype)->array.members);
		} else {
			expr->result = type_store_lookup_pointer(ctx->store,
				inittype, flags);
		}
		break;
	default:
		expr->result = type_store_lookup_pointer(ctx->store,
			inittype, flags);
		break;
	}

	enum type_storage storage = type_dealias(expr->result)->storage;
	switch (storage) {
	case STORAGE_POINTER:
		if (aexpr->alloc.cap != NULL) {
			error(ctx, aexpr->alloc.cap->loc, expr,
					"Allocation with capacity must be of slice type, not %s",
					type_storage_unparse(storage));
			return;
		}
		break;
	case STORAGE_SLICE:
		if (aexpr->alloc.cap != NULL) {
			expr->alloc.cap = xcalloc(sizeof(struct expression), 1);
			check_expression(ctx, aexpr->alloc.cap, expr->alloc.cap, &builtin_type_size);
			if (!type_is_assignable(&builtin_type_size,
					expr->alloc.cap->result)) {
				error(ctx, aexpr->alloc.cap->loc, expr,
					"Allocation capacity must be assignable to size");
				return;
			}
		}
		break;
	default:
		error(ctx, aexpr->loc, expr,
			"Allocation type must be pointer or slice, not %s",
			type_storage_unparse(storage));
		return;
	}
}

static void
check_expr_append(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	assert(aexpr->type == EXPR_APPEND);
	expr->type = EXPR_APPEND;
	expr->result = &builtin_type_void;
	expr->append.is_static = aexpr->append.is_static;
	expr->insert.loc = aexpr->loc;
	expr->append.expr = xcalloc(sizeof(struct expression), 1);
	check_expression(ctx, aexpr->append.expr, expr->append.expr, NULL);
	const struct type *stype = type_dereference(expr->append.expr->result);
	if (!stype) {
		error(ctx, aexpr->access.array->loc, expr,
			"Cannot dereference nullable pointer for append");
		return;
	}
	stype = type_dealias(stype);
	if (stype->storage != STORAGE_SLICE) {
		error(ctx, aexpr->append.expr->loc, expr,
			"append must operate on a slice");
		return;
	}
	if (stype->flags & TYPE_CONST) {
		error(ctx, aexpr->append.expr->loc, expr,
			"append must operate on a mutable slice");
		return;
	}
	assert(expr->append.expr->type == EXPR_ACCESS);
	const struct type *memb = stype->array.members;
	struct append_values **next = &expr->append.values;
	for (struct ast_append_values *avalue = aexpr->append.values; avalue;
			avalue = avalue->next) {
		struct append_values *value = *next =
			xcalloc(sizeof(struct append_values), 1);
		value->expr = 
			xcalloc(sizeof(struct expression), 1);
		check_expression(ctx, avalue->expr, value->expr, memb);
		if (!type_is_assignable(memb, value->expr->result)) {
			error(ctx, avalue->expr->loc, expr,
				"appended value must be assignable to member type");
			return;
		}
		value->expr = lower_implicit_cast(memb, value->expr);
		next = &value->next;
	}
	if (aexpr->append.variadic != NULL) {
		expr->append.variadic = xcalloc(sizeof(struct expression), 1);
		check_expression(ctx, aexpr->append.variadic, expr->append.variadic, stype);
		if (!type_is_assignable(stype, expr->append.variadic->result)) {
			error(ctx, aexpr->append.variadic->loc, expr,
				"appended slice must be assignable to slice type");
			return;
		}
	}
}

static void
check_expr_assert(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_ASSERT;
	expr->result = &builtin_type_void;
	expr->assert.is_static = aexpr->assert.is_static;

	if (aexpr->assert.cond != NULL) {
		expr->assert.cond = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->assert.cond, expr->assert.cond, &builtin_type_bool);
		if (type_dealias(expr->assert.cond->result)->storage != STORAGE_BOOL) {
			error(ctx, aexpr->assert.cond->loc, expr,
				"Assertion condition must be boolean");
			return;
		}
	} else {
		expr->terminates = true;
	}

	expr->assert.message = xcalloc(1, sizeof(struct expression));
	if (aexpr->assert.message != NULL) {
		check_expression(ctx, aexpr->assert.message, expr->assert.message, &builtin_type_str);
		if (expr->assert.message->result->storage != STORAGE_STRING) {
			error(ctx, aexpr->assert.message->loc, expr,
				"Assertion message must be string");
			return;
		}

		assert(expr->assert.message->type == EXPR_CONSTANT);
		size_t n = snprintf(NULL, 0, "%s:%d:%d: ",
			aexpr->loc.path, aexpr->loc.lineno, aexpr->loc.colno);
		size_t s_len = expr->assert.message->constant.string.len;
		char *s = xcalloc(1, n + s_len + 1);
		snprintf(s, n + 1, "%s:%d:%d: ",
			aexpr->loc.path, aexpr->loc.lineno, aexpr->loc.colno);
		memcpy(s+n, expr->assert.message->constant.string.value, s_len);
		s[n + s_len] = '\0';

		expr->assert.message->constant.string.value = s;
		expr->assert.message->constant.string.len = n + s_len;
	} else {
		int n = snprintf(NULL, 0, "Assertion failed: %s:%d:%d",
			aexpr->loc.path, aexpr->loc.lineno, aexpr->loc.colno);
		char *s = xcalloc(1, n + 1);
		snprintf(s, n, "Assertion failed: %s:%d:%d",
			aexpr->loc.path, aexpr->loc.lineno, aexpr->loc.colno);

		expr->assert.message->type = EXPR_CONSTANT;
		expr->assert.message->result = &builtin_type_const_str;
		expr->assert.message->constant.string.value = s;
		expr->assert.message->constant.string.len = n - 1;
	}

	if (expr->assert.is_static) {
		bool cond;
		if (expr->assert.cond != NULL) {
			struct expression out = {0};
			enum eval_result r =
				eval_expr(ctx, expr->assert.cond, &out);
			if (r != EVAL_OK) {
				error(ctx, aexpr->assert.cond->loc, expr,
					"Unable to evaluate static assertion at compile time");
				return;
			}
			assert(out.result->storage == STORAGE_BOOL);
			cond = out.constant.bval;
		} else {
			cond = false;
		}
		// XXX: Should these abort immediately?
		if (!cond) {
			if (aexpr->assert.message != NULL) {
				char format[40];
				snprintf(format, 40, "Static assertion failed %%%lds",
					expr->assert.message->constant.string.len);
				if (aexpr->assert.cond == NULL) {
					error(ctx, aexpr->loc, expr, format,
						expr->assert.message->constant.string.value);
					return;
				} else {
					error(ctx, aexpr->assert.cond->loc,
						expr, format,
						expr->assert.message->constant.string.value);
					return;
				};
			} else {
				error(ctx, aexpr->loc, expr,
					"Static assertion failed");
				return;
			}
		}
	}
}

static void
check_expr_assign(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_ASSIGN;
	expr->result = &builtin_type_void;
	expr->assign.indirect = aexpr->assign.indirect;
	struct expression *object = xcalloc(1, sizeof(struct expression));
	struct expression *value = xcalloc(1, sizeof(struct expression));

	check_expression(ctx, aexpr->assign.object, object, NULL);

	expr->assign.op = aexpr->assign.op;

	if (aexpr->assign.indirect) {
		const struct type *otype = type_dealias(object->result);
		if (otype->storage != STORAGE_POINTER) {
			error(ctx, aexpr->loc, expr,
				"Cannot dereference non-pointer type for assignment");
			return;
		}
		if (otype->pointer.flags & PTR_NULLABLE) {
			error(ctx, aexpr->loc, expr,
				"Cannot dereference nullable pointer type");
			return;
		}
		check_expression(ctx, aexpr->assign.value, value, otype->pointer.referent);
		if (!type_is_assignable(otype->pointer.referent,
				value->result)) {
			error(ctx, aexpr->loc, expr,
				"Value type is not assignable to pointer type");
			return;
		}
		value = lower_implicit_cast(otype->pointer.referent, value);
	} else {
		check_expression(ctx, aexpr->assign.value, value, object->result);
		assert(object->type == EXPR_CONSTANT // If error
				|| object->type == EXPR_ACCESS
				|| object->type == EXPR_SLICE); // Invariant
		if (object->type == EXPR_SLICE) {
			if (expr->assign.op != BIN_LEQUAL) {
				error(ctx, aexpr->assign.object->loc, expr,
					"Slice assignments may not have a binop");
				return;
			}
		}
		if (object->result->flags & TYPE_CONST) {
			error(ctx, aexpr->loc, expr,
					"Cannot assign to const object");
			return;
		}
		if (!type_is_assignable(object->result, value->result)) {
			error(ctx, aexpr->loc, expr,
				"rvalue type is not assignable to lvalue");
			return;
		}
		value = lower_implicit_cast(object->result, value);
	}

	expr->assign.object = object;
	expr->assign.value = value;
}

static const struct type *
type_promote(struct type_store *store,
	const struct type *a,
	const struct type *b)
{
	// Note: we must return either a, b, or NULL
	// TODO: There are likely some improperly handled edge cases around type
	// flags, both here and in the spec
	const struct type *da = type_store_lookup_with_flags(store, a, 0);
	const struct type *db = type_store_lookup_with_flags(store, b, 0);

	if (da == db) {
		const struct type *base = type_store_lookup_with_flags(store, a,
			a->flags | b->flags);
		assert(base == a || base == b);
		return base;
	}

	if (a->storage == STORAGE_ALIAS && b->storage == STORAGE_ALIAS) {
		return NULL;
	}

	da = type_dealias(da);
	db = type_dealias(db);

	if (da == db) {
		return a->storage == STORAGE_ALIAS ? a : b;
	}

	switch (da->storage) {
	case STORAGE_ARRAY:
		if (da->array.length == SIZE_UNDEFINED && da->array.members) {
			return b;
		}
		if (db->array.length == SIZE_UNDEFINED && db->array.members) {
			return a;
		}
		return NULL;
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
		if (!type_is_integer(db) || !type_is_signed(db)
				|| db->size == da->size) {
			return NULL;
		}
		return da->size > db->size ? a : b;
	case STORAGE_U32:
	case STORAGE_U16:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_CHAR:
		if (!type_is_integer(db) || type_is_signed(db)
				|| db->size == da->size) {
			return NULL;
		}
		return da->size > db->size ? a : b;
	case STORAGE_F32:
	case STORAGE_F64:
		if (!type_is_float(db) || db->size == da->size) {
			return NULL;
		}
		return da->size > db->size ? a : b;
	case STORAGE_POINTER:
		if (db->storage == STORAGE_NULL) {
			return a;
		}
		if (db->storage != STORAGE_POINTER) {
			return NULL;
		}
		if (da->pointer.referent->storage == STORAGE_VOID ||
				db->pointer.referent->storage == STORAGE_VOID) {
			return a;
		}
		const struct type *r = type_promote(store,
			da->pointer.referent, db->pointer.referent);
		if (r == da->pointer.referent) {
			return a;
		}
		if (r == db->pointer.referent) {
			return b;
		}
		assert(r == NULL);
		return NULL;
	case STORAGE_NULL:
		assert(db->storage != STORAGE_NULL);
		if (db->storage == STORAGE_POINTER) {
			return b;
		}
		return NULL;
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
		return NULL;
	// Handled above
	case STORAGE_ALIAS:
		assert(0);
	// Invariant
	case STORAGE_FCONST:
	case STORAGE_ICONST:
		assert(0);
	}
	assert(0);
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
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue, builtin_type_for_storage(storage, false));
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue, builtin_type_for_storage(storage, false));
	} else if (!lflex && rflex) {
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue, hint);
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue, lvalue->result);
	} else if (lflex && !rflex) {
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue, hint);
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue, rvalue->result);
	} else {
		check_expression(ctx, aexpr->binarithm.lvalue, lvalue, hint);
		check_expression(ctx, aexpr->binarithm.rvalue, rvalue, hint);
	}

	const struct type *p =
		type_promote(ctx->store, lvalue->result, rvalue->result);
	if (p == NULL) {
		error(ctx, aexpr->loc, expr,
			"Cannot promote lvalue and rvalue");
		return;
	}
	lvalue = lower_implicit_cast(p, lvalue);
	rvalue = lower_implicit_cast(p, rvalue);

	expr->binarithm.lvalue = lvalue;
	expr->binarithm.rvalue = rvalue;

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
			if (initializer->result->storage != STORAGE_ARRAY) {
				error(ctx, aexpr->loc, expr,
					"Cannot infer array length from non-array type");
				return;
			}
			if (initializer->result->array.members
					!= type->array.members) {
				error(ctx, aexpr->loc, expr,
					"Initializer is not assignable to binding type");
				return;
			}
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

		if (type->size == 0 || type->size == SIZE_UNDEFINED) {
			error(ctx, aexpr->loc, expr,
				"Cannot create binding for type of zero or undefined size");
			return;
		}
		if (!type_is_assignable(type, initializer->result)) {
			error(ctx, aexpr->loc, expr,
				"Initializer is not assignable to binding type");
			return;
		}
		binding->initializer = lower_implicit_cast(type, initializer);

		if (abinding->is_static) {
			struct expression *value =
				xcalloc(1, sizeof(struct expression));
			enum eval_result r = eval_expr(
				ctx, binding->initializer, value);
			if (r != EVAL_OK) {
				error(ctx, abinding->initializer->loc, expr,
					"Unable to evaluate static initializer at compile time");
				return;
			}
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
	if (vaargs->result->storage != STORAGE_ARRAY
			|| vaargs->result->array.members != type) {
		error(ctx, val.loc, vaargs,
			"Argument is not assignable to variadic parameter type");
		return;
	}

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
	expr->type = EXPR_CALL;

	struct expression *lvalue = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->call.lvalue, lvalue, NULL);
	expr->call.lvalue = lvalue;

	const struct type *fntype = type_dereference(lvalue->result);
	if (!fntype) {
		error(ctx, aexpr->loc, expr,
			"Cannot dereference nullable pointer type for function call");
		return;
	}
	fntype = type_dealias(fntype);
	if (fntype->storage != STORAGE_FUNCTION) {
		error(ctx, aexpr->loc, expr,
			"Cannot call non-function type");
		return;
	}
	expr->result = fntype->func.result;
	if (fntype->func.flags & FN_NORETURN) {
		expr->terminates = true;
	}

	struct call_argument *arg, **next = &expr->call.args;
	struct ast_call_argument *aarg = aexpr->call.args;
	struct type_func_param *param = fntype->func.params;
	while (param && aarg) {
		arg = *next = xcalloc(1, sizeof(struct call_argument));
		arg->value = xcalloc(1, sizeof(struct expression));

		if (!param->next && fntype->func.variadism == VARIADISM_HARE
				&& !aarg->variadic) {
			lower_vaargs(ctx, aarg, arg->value,
				param->type->array.members);
			arg->value = lower_implicit_cast(param->type, arg->value);
			param = NULL;
			aarg = NULL;
			break;
		}

		check_expression(ctx, aarg->value, arg->value, param->type);

		if (!type_is_assignable(param->type, arg->value->result)) {
			error(ctx, aarg->value->loc, expr,
				"Argument type %s is not assignable to parameter type %s", gen_typename(arg->value->result), gen_typename(param->type));
			return;
		}
		arg->value = lower_implicit_cast(param->type, arg->value);

		aarg = aarg->next;
		param = param->next;
		next = &arg->next;
	}

	if (param && fntype->func.variadism == VARIADISM_HARE) {
		// No variadic arguments, lower to empty slice
		arg = *next = xcalloc(1, sizeof(struct call_argument));
		arg->value = xcalloc(1, sizeof(struct expression));
		lower_vaargs(ctx, NULL, arg->value,
			param->type->array.members);
		arg->value = lower_implicit_cast(param->type, arg->value);
		param = param->next;
	}

	if (aarg) {
		error(ctx, aexpr->loc, expr,
			"Too many parameters for function call");
		return;
	}
	if (param) {
		error(ctx, aexpr->loc, expr,
			"Not enough parameters for function call");
		return;
	}

}

static void
check_expr_cast(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_CAST;
	expr->cast.kind = aexpr->cast.kind;
	struct expression *value = expr->cast.value =
		xcalloc(1, sizeof(struct expression));
	const struct type *secondary = expr->cast.secondary =
		type_store_lookup_atype(ctx->store, aexpr->cast.type);
	// TODO: Instead of allowing errors on casts to void, we should use a
	// different nonterminal
	check_expression(ctx, aexpr->cast.value, value, secondary == &builtin_type_void ? NULL : secondary);

	if (aexpr->cast.kind == C_ASSERTION || aexpr->cast.kind == C_TEST) {
		const struct type *primary = type_dealias(expr->cast.value->result);
		if (primary->storage != STORAGE_TAGGED) {
			error(ctx, aexpr->cast.value->loc, expr,
				"Expected a tagged union type");
			return;
		}
		if (!type_is_castable(value->result, secondary)) {
			error(ctx, aexpr->cast.type->loc, expr,
				"Invalid cast");
			return;
		}
		bool found = false;
		for (const struct type_tagged_union *t = &primary->tagged;
				t; t = t->next) {
			if (t->type->id == secondary->id) {
				found = true;
				break;
			}
		}
		if (!found) {
			error(ctx, aexpr->cast.type->loc, expr,
				"Type is not a valid member of the tagged union type");
			return;
		}
	}

	switch (aexpr->cast.kind) {
	case C_CAST:
		if (!type_is_castable(secondary, value->result)) {
			error(ctx, aexpr->cast.type->loc, expr,
				"Invalid cast");
			return;
		}
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
			if (!type_is_assignable(type, value->result)) {
				error(ctx, item->value->loc, expr,
					"Array members must be of a uniform type");
				return;
			}
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
		if (hint == NULL) {
			error(ctx, aexpr->loc, expr,
				"Cannot expand array for inferred type");
			return;
		}
		if (hint->storage != STORAGE_ARRAY
				|| hint->array.length == SIZE_UNDEFINED
				|| hint->array.length < len) {
			error(ctx, aexpr->loc, expr,
				"Cannot expand array into destination type");
			return;
		}
		expr->result = type_store_lookup_array(ctx->store,
				type, hint->array.length);
	} else {
		if (type == NULL) {
			error(ctx, aexpr->loc, expr,
				"Cannot infer array type from context, try casting it to the desired type");
			return;
		}
		expr->result = type_store_lookup_array(ctx->store, type, len);
	}
}

static const struct type *
lower_constant(const struct type *type, struct expression *expr)
{
	assert(expr->type == EXPR_CONSTANT);
	type = type_dealias(type);
	if (type->storage == STORAGE_TAGGED) {
		const struct type *tag = NULL;
		for (const struct type_tagged_union *tu = &type->tagged; tu;
				tu = tu->next) {
			if (lower_constant(tu->type, expr)) {
				if (tag == NULL) {
					tag = tu->type;
					continue;
				}
				// Ambiguous
				switch (expr->result->storage) {
				case STORAGE_ICONST:
					return lower_constant(&builtin_type_int, expr);
				case STORAGE_FCONST:
					return lower_constant(&builtin_type_f64, expr);
				case STORAGE_RUNE:
					return lower_constant(&builtin_type_rune, expr);
				default:
					assert(0);
				}
			}
		}
		return tag;
	}
	if (type_is_float(type) && type_is_float(expr->result)) {
		return type;
	}
	if (!type_is_integer(type) && type->storage != STORAGE_RUNE) {
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
check_expr_compound(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_COMPOUND;

	struct scope *scope = scope_push(&ctx->scope, SCOPE_COMPOUND);
	scope->hint = hint;
	expr->compound.scope = scope;

	if (aexpr->compound.label) {
		expr->compound.label = strdup(aexpr->compound.label);
		scope->label = strdup(aexpr->compound.label);
	}

	struct expressions *list = &expr->compound.exprs;
	struct expressions **next = &list->next;

	const struct ast_expression_list *alist = &aexpr->compound.list;
	struct expression *lexpr;
	while (alist) {
		lexpr = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, alist->expr, lexpr,
				alist->next ? &builtin_type_void : hint);
		list->expr = lexpr;

		alist = alist->next;
		if (alist) {
			*next = xcalloc(1, sizeof(struct expressions));
			list = *next;
			next = &list->next;
		}
		if (alist && lexpr->terminates) {
			error(ctx, alist->expr->loc, expr,
				"A terminating expression may not be followed by additional expressions");
		}
	}
	if (lexpr->result != &builtin_type_void && hint && hint != &builtin_type_void) {
		fprintf(stderr, "Warning: non-void result ignored at %s:%d\n",
				lexpr->loc.path, lexpr->loc.lineno);
	}

	expr->terminates = lexpr->terminates && lexpr->type != EXPR_YIELD;
	expr->result = type_store_reduce_result(ctx->store, scope->results);

	for (struct yield *yield = scope->yields; yield;) {
		struct expression *lowered = lower_implicit_cast(
				expr->result, *yield->expression);
		if (*yield->expression != lowered) {
			*yield->expression = lowered;
		}

		struct yield *next = yield->next;
		free(yield);
		yield = next;
	}

	assert(expr->result);
	scope_pop(&ctx->scope);
}

static void
check_expr_constant(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_CONSTANT;
	expr->result = builtin_type_for_storage(aexpr->constant.storage, false);

	if (expr->result && expr->result->storage == STORAGE_ICONST) {
		if (hint == NULL) {
			hint = builtin_type_for_storage(STORAGE_INT, false);
		}
		expr->constant.ival = aexpr->constant.ival;
		const struct type *type = lower_constant(hint, expr);
		if (!type) {
			// TODO: This error message is awful
			error(ctx, aexpr->loc, expr,
				"Integer constant out of range");
			return;
		}
		expr->result = type;
	}

	if (expr->result && expr->result->storage == STORAGE_FCONST) {
		if (hint == NULL) {
			hint = builtin_type_for_storage(STORAGE_F64, false);
		}
		expr->constant.fval = aexpr->constant.fval;
		const struct type *type = lower_constant(hint, expr);
		if (!type) {
			// TODO: This error message is awful
			error(ctx, aexpr->loc, expr,
				"Floating constant out of range");
			return;
		}
		expr->result = type;
	}

	if (expr->result && expr->result->storage == STORAGE_RUNE) {
		if (hint == NULL) {
			hint = builtin_type_for_storage(STORAGE_RUNE, false);
		}
		expr->constant.uval = aexpr->constant.uval;
		const struct type *type = lower_constant(hint, expr);
		if (!type) {
			// TODO: This error message is awful
			error(ctx, aexpr->loc, expr,
				"Rune constant out of range");
			return;
		}
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
		expr->constant.fval = aexpr->constant.fval;
		break;
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
	if (ctx->deferring) {
		error(ctx, aexpr->loc, expr,
			"Cannot defer within another defer expression.");
		return;
	}
	expr->type = EXPR_DEFER;
	expr->result = &builtin_type_void;
	expr->defer.deferred = xcalloc(1, sizeof(struct expression));
	ctx->deferring = true;
	check_expression(ctx, aexpr->defer.deferred, expr->defer.deferred, &builtin_type_void);
	ctx->deferring = false;
}

static void
check_expr_delete(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_DELETE;
	expr->delete.is_static = aexpr->delete.is_static;
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
		if (dexpr->access.type != ACCESS_INDEX) {
			error(ctx, aexpr->delete.expr->loc, expr,
				"Deleted expression must be slicing or indexing expression");
			return;
		}
		otype = dexpr->access.array->result;
		break;
	default:
		error(ctx, aexpr->delete.expr->loc, expr,
			"Deleted expression must be slicing or indexing expression");
		return;
	}
	otype = type_dealias(otype);
	while (otype->storage == STORAGE_POINTER) {
		otype = type_dealias(otype->pointer.referent);
	}
	if (otype->storage != STORAGE_SLICE) {
		error(ctx, aexpr->delete.expr->loc, expr,
			"delete must operate on a slice");
		return;
	}
	if (otype->flags & TYPE_CONST) {
		error(ctx, aexpr->delete.expr->loc, expr,
			"delete must operate on a mutable slice");
		return;
	}
}

static void
check_expr_control(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = aexpr->type;
	expr->result = &builtin_type_void;
	expr->control.label = aexpr->control.label;
	expr->terminates = true;

	enum scope_class want;
	switch (expr->type) {
	case EXPR_BREAK:
	case EXPR_CONTINUE:
		want = SCOPE_LOOP;
		break;
	case EXPR_YIELD:
		want = SCOPE_COMPOUND;
		break;
	default:
		abort(); // Invariant
	}

	struct scope *scope = scope_lookup_ancestor(
		ctx->scope, want, aexpr->control.label);
	if (!scope) {
		// XXX: This error message is bad
		error(ctx, aexpr->loc, expr, "No eligible loop for operation");
		return;
	}
	expr->control.scope = scope;

	if (aexpr->control.value) {
		expr->control.value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->control.value,
			expr->control.value, scope->hint);

		struct type_tagged_union *result =
			xcalloc(1, sizeof(struct type_tagged_union));
		result->type = expr->control.value->result;
		result->next = scope->results;
		scope->results = result;

		struct yield *yield = xcalloc(1, sizeof(struct yield));
		yield->expression = &expr->control.value;
		yield->next = scope->yields;
		scope->yields = yield;
	}
}

static void
check_expr_for(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_FOR;
	expr->result = &builtin_type_void;

	struct scope *scope = scope_push(&ctx->scope, SCOPE_LOOP);
	expr->_for.scope = scope;

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
	if (type_dealias(cond->result)->storage != STORAGE_BOOL) {
		error(ctx, aexpr->_for.cond->loc, expr,
			"Expected for condition to be boolean");
		return;
	}

	if (aexpr->_for.afterthought) {
		afterthought = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->_for.afterthought, afterthought, &builtin_type_void);
		expr->_for.afterthought = afterthought;
	}

	body = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_for.body, body, &builtin_type_void);
	expr->_for.body = body;

	scope_pop(&ctx->scope);
}

static void
check_expr_free(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	assert(aexpr->type == EXPR_FREE);
	expr->type = EXPR_FREE;
	expr->free.expr = xcalloc(sizeof(struct expression), 1);
	check_expression(ctx, aexpr->free.expr, expr->free.expr, NULL);
	enum type_storage storage = type_dealias(expr->free.expr->result)->storage;
	if (storage != STORAGE_SLICE && storage != STORAGE_STRING
			&& storage != STORAGE_POINTER) {
		error(ctx, aexpr->free.expr->loc, expr,
			"free must operate on slice, string, or pointer");
		return;
	}
	expr->result = &builtin_type_void;
}

static void
check_expr_if(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_IF;

	struct expression *cond, *true_branch, *false_branch = NULL;

	cond = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_if.cond, cond, &builtin_type_bool);

	true_branch = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_if.true_branch, true_branch, hint);

	if (aexpr->_if.false_branch) {
		false_branch = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->_if.false_branch, false_branch, hint);

		bool tt = true_branch->terminates, ft = false_branch->terminates;
		if (tt && ft) {
			expr->terminates = true;
			expr->result = &builtin_type_void;
		} else if (!tt && ft) {
			expr->result = true_branch->result;
		} else if (tt && !ft) {
			expr->result = false_branch->result;
		} else if (hint && type_is_assignable(hint, true_branch->result)
				&& type_is_assignable(hint, false_branch->result)) {
			expr->result = hint;
		} else {
			struct type_tagged_union _tags = {
				.type = false_branch->result,
				.next = NULL,
			}, tags = {
				.type = true_branch->result,
				.next = &_tags,
			};
			expr->result =
				type_store_reduce_result(ctx->store, &tags);
			if (expr->result == NULL) {
				error(ctx, aexpr->loc, expr,
					"Invalid result type (dangling or ambiguous null)");
				return;
			}
		}
		true_branch = lower_implicit_cast(expr->result, true_branch);
		false_branch = lower_implicit_cast(expr->result, false_branch);
	} else {
		expr->result = &builtin_type_void;
		expr->terminates = false;
	}

	if (type_dealias(cond->result)->storage != STORAGE_BOOL) {
		error(ctx, aexpr->_if.cond->loc, expr,
			"Expected if condition to be boolean");
		return;
	}

	expr->_if.cond = cond;
	expr->_if.true_branch = true_branch;
	expr->_if.false_branch = false_branch;
}

static void
check_expr_insert(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	assert(aexpr->type == EXPR_INSERT);
	expr->type = EXPR_INSERT;
	expr->result = &builtin_type_void;
	expr->insert.is_static = aexpr->insert.is_static;
	expr->insert.loc = aexpr->loc;
	expr->insert.expr = xcalloc(sizeof(struct expression), 1);
	check_expression(ctx, aexpr->insert.expr, expr->insert.expr, NULL);
	assert(expr->insert.expr->type == EXPR_ACCESS
			&& expr->insert.expr->access.type == ACCESS_INDEX);
	const struct type *sltype = expr->insert.expr->access.array->result;
	sltype = type_dereference(sltype);
	if (!sltype) {
		error(ctx, aexpr->access.array->loc, expr,
			"Cannot dereference nullable pointer for insert");
		return;
	}
	sltype = type_dealias(sltype);
	if (sltype->storage != STORAGE_SLICE) {
		error(ctx, aexpr->insert.expr->loc, expr,
			"cannot insert into non-slice type %s",
			type_storage_unparse(type_dealias(sltype)->storage));
		return;
	}
	if (sltype->flags & TYPE_CONST) {
		error(ctx, aexpr->insert.expr->loc, expr,
			"insert must operate on a mutable slice");
		return;
	}
	const struct type *memb = type_dealias(sltype)->array.members;
	struct append_values **next = &expr->insert.values;
	for (struct ast_append_values *avalue = aexpr->insert.values; avalue;
			avalue = avalue->next) {
		struct append_values *value = *next =
			xcalloc(sizeof(struct append_values), 1);
		value->expr = 
			xcalloc(sizeof(struct expression), 1);
		check_expression(ctx, avalue->expr, value->expr, memb);
		if (!type_is_assignable(memb, value->expr->result)) {
			error(ctx, avalue->expr->loc, expr,
				"inserted value must be assignable to member type");
			return;
		}
		value->expr = lower_implicit_cast(memb, value->expr);
		next = &value->next;
	}
	if (aexpr->insert.variadic != NULL) {
		expr->insert.variadic = xcalloc(sizeof(struct expression), 1);
		check_expression(ctx, aexpr->insert.variadic, expr->insert.variadic, sltype);
		if (!type_is_assignable(sltype, expr->insert.variadic->result)) {
			error(ctx, aexpr->insert.variadic->loc, expr,
				"inserted slice type %s must be assignable to slice type",
				type_storage_unparse(expr->insert.variadic->result->storage));
			return;
		}
		expr->insert.variadic = lower_implicit_cast(
				sltype, expr->insert.variadic);
	}
}

static void
check_expr_match(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_MATCH;

	struct expression *value = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->match.value, value, NULL); expr->match.value = value;

	const struct type *type = type_dealias(value->result);
	bool is_ptr = type->storage == STORAGE_POINTER
		&& type->pointer.flags & PTR_NULLABLE;
	if (type->storage != STORAGE_TAGGED && !is_ptr) {
		error(ctx, aexpr->match.value->loc, expr,
			"match value must be tagged union or nullable pointer type");
		return;
	}

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
					if (type->pointer.referent != ctype->pointer.referent) {
						error(ctx, acase->type->loc, expr,
							"Match case on incompatible pointer type");
						return;
					}
					break;
				default:
					error(ctx, acase->type->loc, expr,
						"Invalid type for match case (expected null or pointer type)");
					return;
				}
			} else {
				// TODO: Assign a score to tagged compatibility
				// and choose the branch with the highest score.
				if (!type_is_assignable(type, ctype)) {
					error(ctx, acase->type->loc, expr,
						"Invalid type for match case (match is not assignable to this type)");
					return;
				}
			}
		}

		if (acase->name) {
			assert(ctype);
			struct identifier ident = {
				.name = acase->name,
			};
			struct scope *scope = scope_push(
				&ctx->scope, SCOPE_MATCH);
			_case->object = scope_insert(scope, O_BIND,
				&ident, &ident, ctype, NULL);
		}

		_case->value = xcalloc(1, sizeof(struct expression));
		_case->type = ctype;
		check_expression(ctx, acase->value, _case->value, hint);

		if (acase->name) {
			scope_pop(&ctx->scope);
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
			expr->result = type_store_reduce_result(
				ctx->store, &result_type);
			if (expr->result == NULL) {
				error(ctx, aexpr->loc, expr,
					"Invalid result type (dangling or ambiguous null)");
				return;
			}
		}

		struct match_case *_case = expr->match.cases;
		struct ast_match_case *acase = aexpr->match.cases;
		while (_case) {
			if (!_case->value->terminates && !type_is_assignable(
					expr->result, _case->value->result)) {
				error(ctx, acase->value->loc, expr,
					"Match case is not assignable to result type");
				return;
			}
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
check_expr_measure(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_MEASURE;
	expr->result = &builtin_type_size;
	expr->measure.op = aexpr->measure.op;

	switch (expr->measure.op) {
	case M_LEN:
		expr->measure.value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->measure.value, expr->measure.value, NULL);
		const struct type *atype =
			type_dereference(expr->measure.value->result);
		if (!atype) {
			error(ctx, aexpr->access.array->loc, expr,
				"Cannot dereference nullable pointer for len");
			return;
		}
		enum type_storage vstor = type_dealias(atype)->storage;
		bool valid = vstor == STORAGE_ARRAY || vstor == STORAGE_SLICE
				|| vstor == STORAGE_STRING;
		if (!valid) {
			error(ctx, aexpr->measure.value->loc, expr,
				"len argument must be of an array, slice, or str type");
			return;
		}
		if (expr->measure.value->result->size == SIZE_UNDEFINED) {
			error(ctx, aexpr->measure.value->loc, expr,
				"Cannot take length of array type with undefined length");
			return;
		}
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
	struct expression *lvalue = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->propagate.value, lvalue, hint == &builtin_type_void ? NULL : hint);

	const struct type *intype = lvalue->result;
	if (type_dealias(intype)->storage != STORAGE_TAGGED) {
		error(ctx, aexpr->loc, expr,
			"Cannot use error propagation with non-tagged type");
		return;
	}
	if (!aexpr->propagate.abort) {
		if (ctx->deferring) {
			error(ctx, aexpr->loc, expr,
				"Cannot use error propagation in a defer expression");
			return;
		}
		if (ctx->fntype->func.flags & FN_NORETURN) {
			error(ctx, aexpr->loc, expr,
				"Cannot use error propagation inside @noreturn function");
			return;
		}
	}

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

	if (!return_tagged.type) {
		error(ctx, aexpr->loc, expr,
			"No error can occur here, cannot propagate");
		return;
	}

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

	// Lower to a match expression
	expr->type = EXPR_MATCH;
	expr->match.value = lvalue;

	struct scope *scope = scope_push(&ctx->scope, SCOPE_MATCH);
	struct match_case *case_ok = xcalloc(1, sizeof(struct match_case));
	struct match_case *case_err = xcalloc(1, sizeof(struct match_case));
	struct identifier ok_name = {0}, err_name = {0};

	int n = snprintf(NULL, 0, "ok.%d", ctx->id);
	ok_name.name = xcalloc(n + 1, 1);
	snprintf(ok_name.name, n + 1, "ok.%d", ctx->id);
	++ctx->id;
	const struct scope_object *ok_obj = NULL;
	if (result_type->size != 0 && result_type->size != SIZE_UNDEFINED) {
		ok_obj = scope_insert(scope, O_BIND, &ok_name,
			&ok_name, result_type, NULL);
	}

	n = snprintf(NULL, 0, "err.%d", ctx->id);
	err_name.name = xcalloc(n + 1, 1);
	snprintf(err_name.name, n + 1, "err.%d", ctx->id);
	++ctx->id;
	const struct scope_object *err_obj = NULL;
	if (return_type->size != 0 && return_type->size != SIZE_UNDEFINED) {
		err_obj = scope_insert(scope, O_BIND, &err_name,
			&err_name, return_type, NULL);
	}

	case_ok->type = result_type;
	case_ok->object = ok_obj;
	case_ok->value = xcalloc(1, sizeof(struct expression));
	case_ok->value->result = result_type;
	if (ok_obj) {
		case_ok->value->type = EXPR_ACCESS;
		case_ok->value->access.type = ACCESS_IDENTIFIER;
		case_ok->value->access.object = ok_obj;
	} else {
		case_ok->value->type = EXPR_CONSTANT;
	}

	case_err->type = return_type;
	case_err->object = err_obj;
	case_err->value = xcalloc(1, sizeof(struct expression));

	if (aexpr->propagate.abort) {
		case_err->value->type = EXPR_ASSERT;
		case_err->value->assert.cond = NULL;
		case_err->value->assert.is_static = false;

		int n = snprintf(NULL, 0, "Assertion failed: error occured at %s:%d:%d",
			aexpr->loc.path, aexpr->loc.lineno, aexpr->loc.colno);
		char *s = xcalloc(1, n + 1);
		snprintf(s, n, "Assertion failed: error occured at %s:%d:%d",
			aexpr->loc.path, aexpr->loc.lineno, aexpr->loc.colno);

		case_err->value->assert.message = xcalloc(1, sizeof(struct expression));
		case_err->value->assert.message->type = EXPR_CONSTANT;
		case_err->value->assert.message->result = &builtin_type_const_str;
		case_err->value->assert.message->constant.string.value = s;
		case_err->value->assert.message->constant.string.len = n - 1;
	} else {
		if (!type_is_assignable(ctx->fntype->func.result, return_type)) {
			error(ctx, aexpr->loc, expr,
				"Error type is not assignable to function result type");
			return;
		}

		case_err->value->type = EXPR_RETURN;

		struct expression *rval =
			xcalloc(1, sizeof(struct expression));
		rval->result = return_type;
		if (err_obj != NULL) {
			rval->type = EXPR_ACCESS;
			rval->access.type = ACCESS_IDENTIFIER;
			rval->access.object = err_obj;
		} else {
			rval->type = EXPR_CONSTANT;
		}
		case_err->value->_return.value = lower_implicit_cast(
				ctx->fntype->func.result, rval);
	}
	case_err->value->terminates = true;
	case_err->value->result = &builtin_type_void;

	expr->match.cases = case_ok;
	case_ok->next = case_err;

	scope_pop(&ctx->scope);
	expr->result = result_type;
}

static void
check_expr_return(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	if (ctx->deferring) {
		error(ctx, aexpr->loc, expr,
			"Cannot return inside a defer expression");
		return;
	}
	if (ctx->fntype->func.flags & FN_NORETURN) {
		error(ctx, aexpr->loc, expr,
			"Cannot return inside @noreturn function");
		return;
	}

	expr->type = EXPR_RETURN;
	expr->result = &builtin_type_void;
	expr->terminates = true;

	struct expression *rval = xcalloc(1, sizeof(struct expression));
	if (aexpr->_return.value) {
		check_expression(ctx, aexpr->_return.value, rval, ctx->fntype->func.result);
	} else {
		rval->type = EXPR_CONSTANT;
		rval->result = &builtin_type_void;
	}

	if (!type_is_assignable(ctx->fntype->func.result, rval->result)) {
		error(ctx, aexpr->loc, expr,
			"Return value is not assignable to function result type");
		return;
	}
	if (ctx->fntype->func.result != rval->result) {
		rval = lower_implicit_cast(
			ctx->fntype->func.result, rval);
	}
	expr->_return.value = rval;
}

static void
check_expr_slice(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_SLICE;

	expr->slice.object = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->slice.object, expr->slice.object, NULL);
	const struct type *atype =
		type_dereference(expr->slice.object->result);
	if (!atype) {
		error(ctx, aexpr->slice.object->loc, expr,
			"Cannot dereference nullable pointer for slicing");
		return;
	}
	const struct type *dtype = type_dealias(atype);
	if (dtype->storage != STORAGE_SLICE
			&& dtype->storage != STORAGE_ARRAY) {
		error(ctx, aexpr->slice.object->loc, expr,
			"Cannot slice non-array, non-slice object");
		return;
	}

	const struct type *itype;
	if (aexpr->slice.start) {
		expr->slice.start = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->slice.start, expr->slice.start, &builtin_type_size);
		itype = type_dealias(expr->slice.start->result);
		if (!type_is_integer(itype)) {
			error(ctx, aexpr->slice.start->loc, expr,
				"Cannot use non-integer %s type as slicing operand",
				type_storage_unparse(itype->storage));
			return;
		}
		expr->slice.start = lower_implicit_cast(
			&builtin_type_size, expr->slice.start);
	}

	if (aexpr->slice.end) {
		expr->slice.end = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->slice.end, expr->slice.end, &builtin_type_size);
		itype = type_dealias(expr->slice.end->result);
		if (!type_is_integer(itype)) {
			error(ctx, aexpr->slice.end->loc, expr,
				"Cannot use non-integer %s type as slicing operand",
				type_storage_unparse(itype->storage));
			return;
		}
		expr->slice.end = lower_implicit_cast(
			&builtin_type_size, expr->slice.end);
	} else if (dtype->storage == STORAGE_ARRAY
			&& dtype->array.length == SIZE_UNDEFINED) {
		error(ctx, aexpr->loc, expr,
			"Must have end index on array of undefined length");
		return;
	}

	if (dtype->storage == STORAGE_SLICE) {
		expr->result = atype;
	} else {
		expr->result = type_store_lookup_slice(ctx->store,
			dtype->array.members);
	}
}

static void
check_expr_struct(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_STRUCT;

	const struct type *stype = NULL;
	if (aexpr->_struct.type.name) {
		const struct scope_object *obj = scope_lookup(ctx->scope,
				&aexpr->_struct.type);
		if (!obj) {
			error(ctx, aexpr->loc, expr,
				"Unknown type alias");
			return;
		}
		if (obj->otype != O_TYPE) {
			error(ctx, aexpr->loc, expr,
					"Name does not refer to a type");
			return;
		}
		stype = obj->type;
		if (type_dealias(stype)->storage != STORAGE_STRUCT) {
			error(ctx, aexpr->loc, expr,
				"Object named is not a struct type");
			return;
		}
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
	if (stype == NULL && expr->_struct.autofill) {
		error(ctx, aexpr->loc, expr,
				"Autofill is only permitted for named struct initializers");
		return;
	}

	struct ast_field_value *afield = aexpr->_struct.fields;
	while (afield) {
		const struct type *ftype;
		if (!stype) {
			assert(afield->name); // TODO
			if (!afield->type) {
				error(ctx, aexpr->loc, expr,
					"Unnamed struct must specify field type");
				return;
			}
			tfield->name = afield->name;
			tfield->type = afield->type;
			ftype = type_store_lookup_atype(
				ctx->store, tfield->type);
		} else {
			sexpr->field = type_get_field(type_dealias(stype),
					afield->name);
			if (!sexpr->field) {
				error(ctx, afield->initializer->loc, expr,
					"No field by this name exists for this type");
				return;
			}
			ftype = sexpr->field->type;
		}

		sexpr->value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, afield->initializer, sexpr->value, ftype);

		if (stype) {
			if (!type_is_assignable(sexpr->field->type, sexpr->value->result)) {
				error(ctx, afield->initializer->loc, expr,
					"Initializer is not assignable to struct field");
				return;
			}
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
				expr->result, tfield->name);
			if (!field) {
				// TODO: Use more specific error location
				error(ctx, aexpr->loc, expr,
					"No field by this name exists for this type");
				return;
			}
			if (!type_is_assignable(field->type, sexpr->value->result)) {
				error(ctx, aexpr->loc, expr,
					"Cannot initialize struct field '%s' from value of this type",
					field->name);
				return;
			}
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
}

static void
check_expr_switch(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
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
			if (!type_is_assignable(type_dealias(type),
					type_dealias(value->result))) {
				error(ctx, aopt->value->loc, expr,
					"Invalid type for switch case");
				return;
			}

			enum eval_result r = eval_expr(ctx, value, evaled);
			if (r != EVAL_OK) {
				error(ctx, aopt->value->loc, expr,
					"Unable to evaluate case at compile time");
				return;
			}

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
			expr->result = type_store_reduce_result(
				ctx->store, &result_type);
			if (expr->result == NULL) {
				error(ctx, aexpr->loc, expr,
					"Invalid result type (dangling or ambiguous null)");
				return;
			}
		}

		struct switch_case *_case = expr->_switch.cases;
		struct ast_switch_case *acase = aexpr->_switch.cases;
		while (_case) {
			if (!_case->value->terminates && !type_is_assignable(
					expr->result, _case->value->result)) {
				error(ctx, acase->value->loc, expr,
					"Switch case is not assignable to result type");
				return;
			}
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
		check_expression(ctx, atuple->expr, tuple->value, ttuple ? ttuple->type : NULL);
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
	} else if (hint && type_dealias(hint)->storage == STORAGE_TAGGED) {
		for (const struct type_tagged_union *tu =
				&type_dealias(hint)->tagged;
				tu; tu = tu->next) {
			if (type_dealias(tu->type)->storage != STORAGE_TUPLE) {
				continue;
			}
			const struct type_tuple *ttuple =
				&type_dealias(tu->type)->tuple;
			const struct expression_tuple *etuple = &expr->tuple;
			bool valid = true;
			while (etuple) {
				if (!ttuple || !type_is_assignable(ttuple->type,
						etuple->value->result)) {
					valid = false;
					break;
				}
				ttuple = ttuple->next;
				etuple = etuple->next;
			}
			if (!ttuple && valid) {
				expr->result = type_dealias(tu->type);
				break;
			}
		}
		if (!expr->result) {
			error(ctx, aexpr->loc, expr,
				"Tuple value is not assignable to tagged union hint");
			return;
		}
	} else {
		expr->result = type_store_lookup_tuple(ctx->store, &result);
	}

	ttuple = &type_dealias(expr->result)->tuple;
	struct expression_tuple *etuple = &expr->tuple;
	const struct ast_expression_tuple *atuple = &aexpr->tuple;
	while (etuple) {
		if (!ttuple) {
			error(ctx, atuple->expr->loc, expr,
				"Too many values for tuple type");
			return;
		}
		if (!type_is_assignable(ttuple->type, etuple->value->result)) {
			error(ctx, atuple->expr->loc, expr,
				"Value is not assignable to tuple value type");
			return;
		}
		etuple->value = lower_implicit_cast(ttuple->type, etuple->value);
		etuple = etuple->next;
		atuple = atuple->next;
		ttuple = ttuple->next;
	}
	if (ttuple) {
		error(ctx, aexpr->loc, expr,
			"Too few values for tuple type");
		return;
	}
}

static void
check_expr_unarithm(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_UNARITHM;

	struct expression *operand = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->unarithm.operand, operand, NULL);
	expr->unarithm.operand = operand;
	expr->unarithm.op = aexpr->unarithm.op;

	switch (expr->unarithm.op) {
	case UN_LNOT:
		if (type_dealias(operand->result)->storage != STORAGE_BOOL) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot perform logical NOT (!) on non-boolean type");
			return;
		}
		expr->result = &builtin_type_bool;
		break;
	case UN_BNOT:
		if (!type_is_integer(operand->result)) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot perform binary NOT (~) on non-integer type");
			return;
		}
		if (type_is_signed(operand->result)) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot perform binary NOT (~) on signed type");
			return;
		}
		expr->result = operand->result;
		break;
	case UN_MINUS:
	case UN_PLUS:
		if (!type_is_numeric(operand->result)) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot perform operation on non-numeric type");
			return;
		}
		if (!type_is_signed(operand->result)) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot perform operation on unsigned type");
			return;
		}
		expr->result = operand->result;
		break;
	case UN_ADDRESS:
		expr->result = type_store_lookup_pointer(
			ctx->store, operand->result, 0);
		break;
	case UN_DEREF:
		if (type_dealias(operand->result)->storage != STORAGE_POINTER) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot de-reference non-pointer type");
			return;
		}
		if (type_dealias(operand->result)->pointer.flags
				& PTR_NULLABLE) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot dereference nullable pointer type");
			return;
		}
		expr->result = type_dealias(operand->result)->pointer.referent;
		break;
	}
}

void
check_expression(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->loc = aexpr->loc;

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
	case EXPR_YIELD:
		check_expr_control(ctx, aexpr, expr, hint);
		break;
	case EXPR_CALL:
		check_expr_call(ctx, aexpr, expr, hint);
		break;
	case EXPR_CAST:
		check_expr_cast(ctx, aexpr, expr, hint);
		break;
	case EXPR_COMPOUND:
		check_expr_compound(ctx, aexpr, expr, hint);
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
	case EXPR_INSERT:
		check_expr_insert(ctx, aexpr, expr, hint);
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
	assert(expr->result);
	if (hint && hint->storage == STORAGE_VOID) {
		if ((expr->result->flags & TYPE_ERROR) != 0) {
			error(ctx, aexpr->loc, expr,
				"Cannot ignore error here");
			return;
		}
		if (type_dealias(expr->result)->storage != STORAGE_TAGGED) {
			return;
		}
		const struct type_tagged_union *tu =
			&type_dealias(expr->result)->tagged;
		for (; tu; tu = tu->next) {
			if ((tu->type->flags & TYPE_ERROR) == 0) {
				continue;
			}
			error(ctx, aexpr->loc, expr,
				"Cannot ignore error here");
			return;
		}
	}
}

static struct declaration *
check_const(struct context *ctx,
	const struct ast_global_decl *adecl)
{
	const struct type *type = type_store_lookup_atype(
			ctx->store, adecl->type);
	struct declaration *decl = xcalloc(1, sizeof(struct declaration));
	const struct scope_object *obj = scope_lookup(
			ctx->unit, &adecl->ident);
	assert(obj && obj->otype == O_CONST);
	decl->type = DECL_CONST;
	decl->constant.type = type;
	decl->constant.value = obj->value;
	mkident(ctx, &decl->ident, &adecl->ident);
	return decl;
}

static struct declaration *
check_function(struct context *ctx,
	const struct ast_decl *adecl)
{
	const struct ast_function_decl *afndecl = &adecl->function;
	if ((adecl->function.flags & FN_TEST) && !ctx->is_test) {
		return NULL;
	}

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

	if (!adecl->function.body) {
		return decl; // Prototype
	}

	decl->func.scope = scope_push(&ctx->scope, SCOPE_FUNC);
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
	// TODO: Pass errors up and deal with them at the end of check
	handle_errors(ctx->errors);

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

	scope_pop(&ctx->scope);
	ctx->fntype = NULL;
	return decl;
}

static struct declaration *
check_global(struct context *ctx,
	const struct ast_global_decl *adecl)
{
	if (!adecl->init) {
		return NULL; // Forward declaration
	}

	const struct type *type = type_store_lookup_atype(
			ctx->store, adecl->type);

	// TODO: Free initialier
	struct expression *initializer =
		xcalloc(1, sizeof(struct expression));
	check_expression(ctx, adecl->init, initializer, type);
	// TODO: Pass errors up and deal with them at the end of check
	handle_errors(ctx->errors);

	expect(&adecl->init->loc,
		type_is_assignable(type, initializer->result),
		"Constant type is not assignable from initializer type");
	initializer = lower_implicit_cast(type, initializer);

	struct expression *value =
		xcalloc(1, sizeof(struct expression));
	enum eval_result r = eval_expr(ctx, initializer, value);
	expect(&adecl->init->loc, r == EVAL_OK,
		"Unable to evaluate global initializer at compile time");

	struct declaration *decl = xcalloc(1, sizeof(struct declaration));
	decl->type = DECL_GLOBAL;
	decl->global.type = type;
	decl->global.value = value;

	if (adecl->symbol) {
		decl->ident.name = strdup(adecl->symbol);
		decl->symbol = strdup(adecl->symbol);
	} else {
		mkident(ctx, &decl->ident, &adecl->ident);
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
	while (adecls) {
		struct declaration *decl = NULL;
		const struct ast_decl *adecl = &adecls->decl;
		switch (adecl->decl_type) {
		case AST_DECL_CONST:
			for (const struct ast_global_decl *c = &adecl->constant;
					c; c = c->next) {
				decl = check_const(ctx, c);
				struct declarations *decls = *next =
					xcalloc(1, sizeof(struct declarations));
				decl->exported = adecl->exported;
				decls->decl = decl;
				next = &decls->next;
			}
			decl = NULL;
			break;
		case AST_DECL_FUNC:
			decl = check_function(ctx, adecl);
			break;
		case AST_DECL_GLOBAL:
			for (const struct ast_global_decl *g = &adecl->global;
					g; g = g->next) {
				decl = check_global(ctx, g);
				if (decl == NULL) {
					continue;
				}
				struct declarations *decls = *next =
					xcalloc(1, sizeof(struct declarations));
				decl->exported = adecl->exported;
				decls->decl = decl;
				next = &decls->next;
			}
			decl = NULL;
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
	return next;
}

static bool expr_is_specified(struct context *ctx,
	const struct ast_expression *aexpr);

static bool
type_is_specified(struct context *ctx, const struct ast_type *atype)
{
	if (!atype) {
		return true;
	}

	switch (atype->storage) {
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_ICONST:
	case STORAGE_INT:
	case STORAGE_NULL:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_STRING:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VOID:
		return true;
	case STORAGE_ALIAS:
		return scope_lookup(ctx->scope, &atype->alias) != NULL;
	case STORAGE_ARRAY:
		return type_is_specified(ctx, atype->array.members)
			&& expr_is_specified(ctx, atype->array.length);
	case STORAGE_SLICE:
		return true;
	case STORAGE_ENUM:
		for (struct ast_enum_field *field = atype->_enum.values;
				field; field = field->next) {
			if (!expr_is_specified(ctx, field->value)) {
				return false;
			}
		}
		return true;
	case STORAGE_FUNCTION:
		for (struct ast_function_parameters *param = atype->func.params;
				param; param = param->next) {
			if (!type_is_specified(ctx, param->type)) {
				return false;
			}
		}
		return type_is_specified(ctx, atype->func.result);
	case STORAGE_POINTER:
		return true;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		for (const struct ast_struct_union_type *stype =
				&atype->struct_union;
				stype; stype = stype->next) {
			if (!expr_is_specified(ctx, stype->offset)) {
				return false;
			}
			if (!type_is_specified(ctx, stype->type)) {
				return false;
			}
		}
		return true;
	case STORAGE_TAGGED:
		for (const struct ast_tagged_union_type *ttype =
				&atype->tagged_union;
				ttype; ttype = ttype->next) {
			if (!type_is_specified(ctx, ttype->type)) {
				return false;
			}
		}
		return true;
	case STORAGE_TUPLE:
		for (const struct ast_tuple_type *ttype = &atype->tuple;
				ttype; ttype = ttype->next) {
			if (!type_is_specified(ctx, ttype->type)) {
				return false;
			}
		}
		return true;
	}
	assert(0); // Unreachable
}

static bool
expr_is_specified(struct context *ctx, const struct ast_expression *aexpr)
{
	if (!aexpr) {
		return true;
	}

	switch (aexpr->type) {
	case EXPR_ACCESS:
		switch (aexpr->access.type) {
		case ACCESS_IDENTIFIER:
			// XXX: Is this right?
			//return scope_lookup(ctx->scope, &aexpr->access.ident);
			return true;
		case ACCESS_INDEX:
			return expr_is_specified(ctx, aexpr->access.array)
				&& expr_is_specified(ctx, aexpr->access.index);
		case ACCESS_FIELD:
			return expr_is_specified(ctx, aexpr->access._struct);
		case ACCESS_TUPLE:
			return expr_is_specified(ctx, aexpr->access.tuple)
				&& expr_is_specified(ctx, aexpr->access.value);
		}
		assert(0);
	case EXPR_ALLOC:
		return expr_is_specified(ctx, aexpr->alloc.expr)
			&& expr_is_specified(ctx, aexpr->alloc.cap);
	case EXPR_APPEND:
		for (const struct ast_append_values *value =
				aexpr->append.values;
				value; value = value->next) {
			if (!expr_is_specified(ctx, value->expr)) {
				return false;
			}
		}
		return expr_is_specified(ctx, aexpr->append.expr)
			&& expr_is_specified(ctx, aexpr->append.variadic);
	case EXPR_ASSERT:
		return expr_is_specified(ctx, aexpr->assert.cond)
			&& expr_is_specified(ctx, aexpr->assert.message);
	case EXPR_ASSIGN:
		return expr_is_specified(ctx, aexpr->assign.object)
			&& expr_is_specified(ctx, aexpr->assign.value);
	case EXPR_BINARITHM:
		return expr_is_specified(ctx, aexpr->binarithm.lvalue)
			&& expr_is_specified(ctx, aexpr->binarithm.rvalue);
	case EXPR_BINDING:
		for (const struct ast_expression_binding *binding =
				&aexpr->binding;
				binding; binding = binding->next) {
			if (!expr_is_specified(ctx, binding->initializer)) {
				return false;
			}
			if (!type_is_specified(ctx, binding->type)) {
				return false;
			}
		}
		return true;
	case EXPR_COMPOUND:
		for (const struct ast_expression_list *list = &aexpr->compound.list;
				list; list = list->next) {
			if (!expr_is_specified(ctx, list->expr)) {
				return false;
			}
		}
		return true;
	case EXPR_BREAK:
	case EXPR_CONTINUE:
		return true;
	case EXPR_CALL:
		for (struct ast_call_argument *arg = aexpr->call.args; arg;
				arg = arg->next) {
			if (!expr_is_specified(ctx, arg->value)) {
				return false;
			}
		}
		return expr_is_specified(ctx, aexpr->call.lvalue);
	case EXPR_CAST:
		return expr_is_specified(ctx, aexpr->cast.value)
			&& type_is_specified(ctx, aexpr->cast.type);
	case EXPR_CONSTANT:
		if (aexpr->constant.storage == STORAGE_ARRAY) {
			for (struct ast_array_constant *aconst =
					aexpr->constant.array;
					aconst; aconst = aconst->next) {
				if (!expr_is_specified(ctx, aconst->value)) {
					return false;
				}
			}
		}
		return true;
	case EXPR_DEFER:
		return expr_is_specified(ctx, aexpr->defer.deferred);
	case EXPR_DELETE:
		return expr_is_specified(ctx, aexpr->delete.expr);
	case EXPR_FOR:
		return expr_is_specified(ctx, aexpr->_for.bindings)
			&& expr_is_specified(ctx, aexpr->_for.cond)
			&& expr_is_specified(ctx, aexpr->_for.afterthought)
			&& expr_is_specified(ctx, aexpr->_for.body);
	case EXPR_FREE:
		return expr_is_specified(ctx, aexpr->free.expr);
	case EXPR_IF:
		return expr_is_specified(ctx, aexpr->_if.cond)
			&& expr_is_specified(ctx, aexpr->_if.true_branch)
			&& expr_is_specified(ctx, aexpr->_if.false_branch);
	case EXPR_INSERT:
		assert(0); // TODO
	case EXPR_MATCH:
		for (struct ast_match_case *mcase = aexpr->match.cases; mcase;
				mcase = mcase->next) {
			if (!type_is_specified(ctx, mcase->type)) {
				return false;
			}
			if (!expr_is_specified(ctx, mcase->value)) {
				return false;
			}
		}
		return expr_is_specified(ctx, aexpr->match.value);
	case EXPR_MEASURE:
		switch (aexpr->measure.op) {
		case M_LEN:
			return expr_is_specified(ctx, aexpr->measure.value);
		case M_SIZE:
			return type_is_specified(ctx, aexpr->measure.type);
		case M_OFFSET:
			assert(0); // TODO
		}
		assert(0);
	case EXPR_PROPAGATE:
		return expr_is_specified(ctx, aexpr->propagate.value);
	case EXPR_RETURN:
		return expr_is_specified(ctx, aexpr->_return.value);
	case EXPR_SLICE:
		return expr_is_specified(ctx, aexpr->slice.object)
			&& expr_is_specified(ctx, aexpr->slice.start)
			&& expr_is_specified(ctx, aexpr->slice.end);
	case EXPR_STRUCT:
		for (struct ast_field_value *field = aexpr->_struct.fields;
				field; field = field->next) {
			if (field->type && !type_is_specified(ctx, field->type)) {
				return false;
			}
			if (!expr_is_specified(ctx, field->initializer)) {
				return false;
			}
		}
		if (aexpr->_struct.type.name) {
			if (!scope_lookup(ctx->scope, &aexpr->_struct.type)) {
				return false;
			}
		}
		return true;
	case EXPR_SWITCH:
		for (struct ast_switch_case *scase = aexpr->_switch.cases;
				scase; scase =scase->next) {
			for (struct ast_case_option *opt = scase->options;
					opt; opt = opt->next) {
				if (!expr_is_specified(ctx, opt->value)) {
					return false;
				}
			}
			if (!expr_is_specified(ctx, scase->value)) {
				return false;
			}
		}
		return expr_is_specified(ctx, aexpr->_switch.value);
	case EXPR_TUPLE:
		for (const struct ast_expression_tuple *tuple = &aexpr->tuple;
				tuple; tuple = tuple->next) {
			if (!expr_is_specified(ctx, tuple->expr)) {
				return false;
			}
		}
		return true;
	case EXPR_UNARITHM:
		return expr_is_specified(ctx, aexpr->unarithm.operand);
	case EXPR_YIELD:
		return expr_is_specified(ctx, aexpr->control.value);
	}
	assert(0); // Unreachable
}

static bool
scan_const(struct context *ctx, const struct ast_global_decl *decl)
{
	// TODO: Get rid of this once the type store bubbles up errors
	for (const struct ast_global_decl *d = decl; d; d = d->next) {
		if (!type_is_specified(ctx, d->type)
				|| !expr_is_specified(ctx, d->init)) {
			return false;
		}
	}
	assert(!decl->symbol); // Invariant

	const struct type *type = type_store_lookup_atype(
			ctx->store, decl->type);
	// TODO: Free the initializer
	struct expression *initializer =
		xcalloc(1, sizeof(struct expression));
	check_expression(ctx, decl->init, initializer, type);
	if (ctx->errors != NULL) {
		return false;
	}

	expect(&decl->init->loc, type_is_assignable(type, initializer->result),
		"Constant type is not assignable from initializer type");
	initializer = lower_implicit_cast(type, initializer);

	struct expression *value =
		xcalloc(1, sizeof(struct expression));
	enum eval_result r = eval_expr(ctx, initializer, value);
	expect(&decl->init->loc, r == EVAL_OK,
		"Unable to evaluate constant initializer at compile time");

	struct identifier ident = {0};
	mkident(ctx, &ident, &decl->ident);
	scope_insert(ctx->unit, O_CONST, &ident, &decl->ident, type, value);
	return true;
}

static bool
scan_function(struct context *ctx, const struct ast_function_decl *decl)
{
	if ((decl->flags & FN_TEST) && !ctx->is_test) {
		return true;
	}
	const struct ast_type fn_atype = {
		.storage = STORAGE_FUNCTION,
		.flags = TYPE_CONST,
		.func = decl->prototype,
	};
	// TODO: Get rid of this once the type store bubbles up errors
	// TODO: Do we want to do something on !expr_is_specified(ctx, decl->body)?
	if (!type_is_specified(ctx, &fn_atype)) {
		return false;
	}
	const struct type *fntype = type_store_lookup_atype(
			ctx->store, &fn_atype);
	assert(fntype);

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
	return true;
}

static bool
scan_global(struct context *ctx, const struct ast_global_decl *decl)
{
	// TODO: Get rid of this once the type store bubbles up errors
	if (!type_is_specified(ctx, decl->type)
			|| !expr_is_specified(ctx, decl->init)) {
		return false;
	}

	const struct type *type = type_store_lookup_atype(
			ctx->store, decl->type);
	assert(type);

	if (decl->type->storage == STORAGE_ARRAY
			&& decl->type->array.contextual) {
		// TODO: Free initialier
		struct expression *initializer =
			xcalloc(1, sizeof(struct expression));
		check_expression(ctx, decl->init, initializer, type);
		if (ctx->errors != NULL) {
			return false;
		}
		expect(&decl->init->loc,
			initializer->result->storage == STORAGE_ARRAY,
			"Cannot infer array length from non-array type");
		expect(&decl->init->loc,
			initializer->result->array.members == type->array.members,
			"Initializer is not assignable to binding type");
		type = initializer->result;
	}

	struct identifier ident = {0};
	if (decl->symbol) {
		ident.name = strdup(decl->symbol);
	} else {
		mkident(ctx, &ident, &decl->ident);
	}
	scope_insert(ctx->unit, O_DECL, &ident, &decl->ident, type, NULL);
	return true;
}

static bool
scan_type(struct context *ctx, const struct ast_type_decl *decl, bool exported)
{
	// TODO: Get rid of this once the type store bubbles up errors
	if (!type_is_specified(ctx, decl->type)) {
		return false;
	}
	const struct type *type =
		type_store_lookup_atype(ctx->store, decl->type);

	struct identifier ident = {0};
	mkident(ctx, &ident, &decl->ident);

	const struct type *alias =
		type_store_lookup_alias(ctx->store, &ident, type, exported);
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
	return true;
}

static bool
scan_declaration(struct context *ctx, const struct ast_decl *decl)
{
	switch (decl->decl_type) {
	case AST_DECL_CONST:
		for (const struct ast_global_decl *c = &decl->constant; c;
				c = c->next) {
			if (!scan_const(ctx, c)) {
				return false;
			}
		}
		return true;
	case AST_DECL_FUNC:
		return scan_function(ctx, &decl->function);
	case AST_DECL_GLOBAL:
		for (const struct ast_global_decl *g = &decl->global; g;
				g = g->next) {
			if (!scan_global(ctx, g)) {
				return false;
			}
		}
		return true;
	case AST_DECL_TYPE:
		return scan_type(ctx, &decl->type, decl->exported);
	}
	assert(0); // Unreachable
}

static struct ast_decls *
scan_declarations(struct context *ctx, const struct ast_decls *decls)
{
	struct ast_decls *next = NULL;
	bool found = false;
	while (decls || next) {
		if (!decls) {
			if (!found) {
				return next;
			}
			decls = next;
			next = NULL;
			found = false;
		}
		if (scan_declaration(ctx, &decls->decl)) {
			found = true;
		} else {
			// TODO: Free this
			struct ast_decls *cur =
				xcalloc(sizeof(struct ast_decls), 1);
			memcpy(cur, decls, sizeof(struct ast_decls));
			cur->next = next;
			next = cur;
		}
		decls = decls->next;
	}
	return next;
}

static void
load_import(struct context *ctx, struct ast_imports *import,
	struct type_store *ts, struct scope *scope)
{
	struct scope *mod = module_resolve(ctx->modcache,
			ctx->defines, &import->ident, ts);

	switch (import->mode) {
	case AST_IMPORT_IDENTIFIER:
		for (struct scope_object *obj = mod->objects;
				obj; obj = obj->lnext) {
			scope_insert(scope, obj->otype, &obj->ident,
				&obj->name, obj->type, obj->value);
			if (obj->name.ns && obj->name.ns->ns) {
				struct identifier ns2 = {
					.name = NULL,
					.ns = NULL,
				};
				struct identifier ns = {
					.name = obj->name.ns->name,
					.ns = NULL
				};
				struct identifier name = {
					.name = obj->name.name,
					.ns = &ns,
				};
				if (type_dealias(obj->type)->storage == STORAGE_ENUM
						&& obj->otype == O_CONST) {
					ns2.name = obj->name.ns->ns->name;
					ns.ns = &ns2;
				};
				scope_insert(scope, obj->otype, &obj->ident,
					&name, obj->type, obj->value);
			}
		}
		break;
	case AST_IMPORT_ALIAS:
		for (struct scope_object *obj = mod->objects;
				obj; obj = obj->lnext) {
			struct identifier ns = {
				.name = obj->name.ns->name,
				.ns = import->alias,
			};
			struct identifier name = {
				.name = obj->name.name,
				.ns = import->alias,
			};
			if (type_dealias(obj->type)->storage == STORAGE_ENUM
					&& obj->otype == O_CONST) {
				name.ns = &ns;
			};
			scope_insert(scope, obj->otype, &obj->ident,
				&name, obj->type, obj->value);
		}
		break;
	case AST_IMPORT_MEMBERS:
		for (struct ast_imports *member = import->members;
				member; member = member->next) {
			struct identifier name = {
				.name = member->ident.name,
				.ns = NULL,
			};
			struct identifier ident = {
				.name = member->ident.name,
				.ns = &import->ident,
			};
			const struct scope_object *obj = scope_lookup(mod, &ident);
			char buf[1024];
			identifier_unparse_static(&ident, buf, sizeof(buf));
			expect(&member->loc, obj, "Unknown object '%s'", buf);
			scope_insert(scope, obj->otype, &obj->ident,
				&name, obj->type, obj->value);
			if (type_dealias(obj->type)->storage != STORAGE_ENUM
					|| obj->otype != O_TYPE) {
				continue;
			};
			for (struct scope_object *o = mod->objects;
					o; o = o->lnext) {
				if (!identifier_eq(o->name.ns, &ident)) {
					continue;
				};
				struct identifier n = {
					.name = o->name.name,
					.ns = &name,
				};
				scope_insert(scope, o->otype, &o->ident,
					&n, o->type, o->value);
			};
		}
		break;
	}
}

struct scope *
check_internal(struct type_store *ts,
	struct modcache **cache,
	bool is_test,
	struct define *defines,
	const struct ast_unit *aunit,
	struct unit *unit,
	bool scan_only)
{
	struct context ctx = {0};
	ctx.ns = unit->ns;
	ctx.is_test = is_test;
	ctx.store = ts;
	ctx.store->check_context = &ctx;
	ctx.modcache = cache;
	ctx.defines = defines;
	ctx.next = &ctx.errors;

	// Top-level scope management involves:
	//
	// - Creating a top-level scope for the whole unit, to which
	//   declarations are added.
	// - Creating a scope for each sub-unit, and populating it with imports.
	// 
	// Further down the call frame, subsequent functions will create
	// sub-scopes for each declaration, expression-list, etc.
	ctx.unit = scope_push(&ctx.scope, SCOPE_UNIT);

	// Install defines (-D on the command line)
	// XXX: This duplicates a lot of code with scan_const
	for (struct define *def = defines; def; def = def->next) {
		struct location loc = {
			.path = "-D", .lineno = 1, .colno = 1,
		};
		const struct type *type = type_store_lookup_atype(
				ctx.store, def->type);
		expect(&loc, type != NULL, "Unable to resolve type");
		struct expression *initializer =
			xcalloc(1, sizeof(struct expression));
		check_expression(&ctx, def->initializer, initializer, type);
		// TODO: This could be more detailed
		expect(&loc, ctx.errors == NULL, "Invalid initializer");
		expect(&loc, type_is_assignable(type, initializer->result),
			"Constant type is not assignable from initializer type");
		initializer = lower_implicit_cast(type, initializer);
		struct expression *value =
			xcalloc(1, sizeof(struct expression));
		enum eval_result r = eval_expr(&ctx, initializer, value);
		expect(&loc, r == EVAL_OK,
			"Unable to evaluate constant initializer at compile time");
		scope_insert(ctx.unit, O_CONST,
			&def->ident, &def->ident, type, value);
	}

	struct scopes *subunit_scopes;
	struct scopes **next = &subunit_scopes;
	struct imports **inext = &unit->imports;

	struct unresolveds {
		struct scope *scope;
		const struct ast_decls *unresolved;
		struct unresolveds *next;
	};
	struct unresolveds *cur = NULL, *unresolved = NULL;

	// First pass populates the imports
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		scope_push(&ctx.scope, SCOPE_SUBUNIT);

		for (struct ast_imports *imports = su->imports;
				imports; imports = imports->next) {
			load_import(&ctx, imports, ts, ctx.scope);

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

		struct unresolveds *new =
			xcalloc(sizeof(struct unresolveds), 1);
		new->unresolved = su->decls;
		new->next = cur;
		cur = new;

		*next = xcalloc(1, sizeof(struct scopes));
		new->scope = (*next)->scope = scope_pop(&ctx.scope);
		next = &(*next)->next;
	}

	// Second pass populates the type graph
	bool found = false;
	while (cur || unresolved) {
		if (!cur) {
			if (!found) {
				handle_errors(ctx.errors);
			}
			cur = unresolved;
			unresolved = NULL;
			found = false;
		}
		struct errors *error = ctx.errors;
		while (error) {
			struct errors *next = error->next;
			free(error);
			error = next;
		}
		ctx.errors = NULL;
		ctx.next = &ctx.errors;
		ctx.scope = cur->scope;
		ctx.store->check_context = &ctx;
		struct ast_decls *left =
			scan_declarations(&ctx, cur->unresolved);
		if (left) {
			struct unresolveds *new =
				xcalloc(sizeof(struct unresolveds), 1);
			new->scope = cur->scope;
			new->unresolved = left;
			new->next = unresolved;
			unresolved = new;
		}

		size_t old_len = 0, new_len = 0;
		for (const struct ast_decls *old = cur->unresolved; old;
				old = old->next) {
			old_len++;
		}
		for (struct ast_decls *new = left; new;
				new = new->next) {
			new_len++;
		}
		if (new_len < old_len) {
			found = true;
		}

		struct unresolveds *tmp = cur;
		cur = tmp->next;
		free(tmp);
	}

	if (scan_only) {
		return ctx.unit;
	}

	struct errors *error = ctx.errors;
	while (error) {
		struct errors *next = error->next;
		free(error);
		error = next;
	}
	ctx.errors = NULL;
	ctx.next = &ctx.errors;

	// Third pass populates the expression graph
	struct scopes *scope = subunit_scopes;
	struct declarations **next_decl = &unit->declarations;
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		ctx.scope = scope->scope;
		next_decl = check_declarations(&ctx, su->decls, next_decl);
		scope = scope->next;
	}

	if (!unit->declarations) {
		fprintf(stderr, "Error: module contains no declarations\n");
		exit(EXIT_FAILURE);
	}

	return ctx.unit;
}

struct scope *
check(struct type_store *ts,
	bool is_test,
	struct define *defines,
	const struct ast_unit *aunit,
	struct unit *unit)
{
	struct modcache *modcache[MODCACHE_BUCKETS];
	memset(modcache, 0, sizeof(modcache));
	return check_internal(ts, modcache, is_test, defines, aunit, unit, false);
}
