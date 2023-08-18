#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
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

void
mkident(struct context *ctx, struct identifier *out, const struct identifier *in,
		const char *symbol)
{
	if (symbol) {
		out->name = xstrdup(symbol);
		return;
	}
	identifier_dup(out, in);
	if (ctx->ns && !in->ns) {
		out->ns = xcalloc(1, sizeof(struct identifier));
		identifier_dup(out->ns, ctx->ns);
	}
}

size_t
mkstrconst(struct expression *expr, const char *fmt, ...)
{
	va_list ap1, ap2;
	va_start(ap1, fmt);
	size_t n = vsnprintf(NULL, 0, fmt, ap1);
	va_end(ap1);
	char *s = xcalloc(1, n + 1);
	va_start(ap2, fmt);
	vsnprintf(s, n + 1, fmt, ap2);
	va_end(ap2);

	*expr = (struct expression) {
		.type = EXPR_CONSTANT,
		.terminates = false,
		.result = &builtin_type_const_str,
	};
	expr->constant.string.value = s;
	expr->constant.string.len = n;
	return n;
}

static char *
gen_typename(const struct type *type)
{
	size_t sz = 0;
	char *ptr = NULL;
	FILE *f = open_memstream(&ptr, &sz);
	if (f == NULL) {
		xfprintf(stderr, "Unable to open memstream: %s\n",
			strerror(errno));
		exit(EXIT_FAILURE);
	}
	emit_type(type, f);
	fclose(f);
	return ptr;
}

static void
handle_errors(struct errors *errors)
{
	struct errors *error = errors;
	while (error) {
		xfprintf(stderr, "%s:%d:%d: error: %s\n", sources[error->loc.file],
			error->loc.lineno, error->loc.colno, error->msg);
		errline(sources[error->loc.file], error->loc.lineno, error->loc.colno);
		struct errors *next = error->next;
		free(error);
		error = next;
	}
	if (errors) {
		exit(EXIT_FAILURE);
	}
}

static void
mkerror(const struct location loc, struct expression *expr)
{
	expr->type = EXPR_CONSTANT;
	expr->result = &builtin_type_error;
	expr->constant.uval = 0; // XXX: ival?
	expr->terminates = false;
	expr->loc = loc;
}

static void
verror(struct context *ctx, const struct location loc, struct expression *expr,
		char *fmt, va_list ap)
{
	if (expr) {
		mkerror(loc, expr);
	}

	va_list copy;
	va_copy(copy, ap);
	size_t sz = vsnprintf(NULL, 0, fmt, copy);
	va_end(copy);

	char *msg = xcalloc(1, sz + 1);
	vsnprintf(msg, sz + 1, fmt, ap);

	struct errors *next = *ctx->next = xcalloc(1, sizeof(struct errors));
	next->loc = loc;
	next->msg = msg;
	ctx->next = &next->next;
}

static void
error(struct context *ctx, const struct location loc, struct expression *expr,
		char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	verror(ctx, loc, expr, fmt, ap);
	va_end(ap);
}

noreturn void
error_norec(struct context *ctx, const struct location loc, char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	verror(ctx, loc, NULL, fmt, ap);
	va_end(ap);

	handle_errors(ctx->errors);
	abort();
}

static struct expression *
lower_implicit_cast(struct context *ctx,
		const struct type *to, struct expression *expr)
{
	if (to == expr->result || expr->terminates) {
		return expr;
	}
	
	if (type_dealias(ctx, to)->storage == STORAGE_SLICE &&
		expr->result->storage == STORAGE_ARRAY &&
		expr->result->array.expandable) {
		return expr;
	}

	if (type_dealias(ctx, to)->storage == STORAGE_TAGGED) {
		const struct type *interim =
			tagged_select_subtype(ctx, to, expr->result, true);
		if (interim) {
			expr = lower_implicit_cast(ctx, interim, expr);
		}
	}

	struct expression *cast = xcalloc(1, sizeof(struct expression));
	cast->type = EXPR_CAST;
	cast->result = cast->cast.secondary = to;
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

	const struct scope_object *obj = NULL;
	switch (expr->access.type) {
	case ACCESS_IDENTIFIER:
		obj = scope_lookup(ctx->scope, &aexpr->access.ident);
		if (!obj) {
			char buf[1024];
			identifier_unparse_static(&aexpr->access.ident,
				buf, sizeof(buf));
			error(ctx, aexpr->loc, expr,
				"Unknown object '%s'", buf);
			return;
		}
		wrap_resolver(ctx, obj, resolve_decl);

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
			if (type_dealias(ctx, obj->type)->storage != STORAGE_VOID) {
				error(ctx, aexpr->loc, expr,
					"Cannot use non-void type alias '%s' as constant",
					identifier_unparse(&obj->type->alias.ident));
				return;
			}
			expr->type = EXPR_CONSTANT;
			expr->result = obj->type;
			break;
		case O_SCAN:
			assert(0); // handled above
		}
		break;
	case ACCESS_INDEX:
		expr->access.array = xcalloc(1, sizeof(struct expression));
		expr->access.index = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->access.array, expr->access.array, NULL);
		check_expression(ctx, aexpr->access.index, expr->access.index, &builtin_type_size);
		const struct type *atype =
			type_dereference(ctx, expr->access.array->result);
		if (!atype) {
			error(ctx, aexpr->access.array->loc, expr,
				"Cannot dereference nullable pointer for indexing");
			return;
		}
		atype = type_dealias(ctx, atype);
		if (atype->storage == STORAGE_ERROR) {
			mkerror(aexpr->access.array->loc, expr);
			return;
		};
		const struct type *itype =
			type_dealias(ctx, expr->access.index->result);
		if (atype->storage != STORAGE_ARRAY
				&& atype->storage != STORAGE_SLICE) {
			error(ctx, aexpr->access.array->loc, expr,
				"Can only index into array or slice object, but got %s",
				type_storage_unparse(atype->storage));
			return;
		}
		if (atype->storage == STORAGE_SLICE
				&& atype->array.members->storage == STORAGE_VOID) {
			error(ctx, aexpr->access.array->loc, expr,
				"Cannot use index into slice of void");
			return;
		}
		if (!type_is_integer(ctx, itype)) {
			error(ctx, aexpr->access.index->loc, expr,
				"Cannot use non-integer %s type as slice/array index",
				type_storage_unparse(itype->storage));
			return;
		}
		expr->access.index = lower_implicit_cast(ctx, 
			&builtin_type_size, expr->access.index);
		expr->result = type_store_lookup_with_flags(ctx->store,
			atype->array.members, atype->flags | atype->array.members->flags);

		// Compile-time bounds check
		if (atype->storage == STORAGE_ARRAY
				&& atype->array.length != SIZE_UNDEFINED) {
			struct expression *evaled = xcalloc(1, sizeof(struct expression));
			enum eval_result r = eval_expr(ctx, expr->access.index, evaled);
			if (r == EVAL_OK) {
				if (evaled->constant.uval >= atype->array.length) {
					error(ctx, aexpr->loc, expr,
						"Index must not be greater than array length");
					free(evaled);
					return;
				}
				expr->access.bounds_checked = true;
			}
			free(evaled);
		}

		break;
	case ACCESS_FIELD:
		expr->access._struct = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->access._struct, expr->access._struct, NULL);
		const struct type *stype =
			type_dereference(ctx, expr->access._struct->result);
		if (!stype) {
			error(ctx, aexpr->access._struct->loc, expr,
				"Cannot dereference nullable pointer for field selection");
			return;
		}
		stype = type_dealias(ctx, stype);
		if (stype->storage == STORAGE_ERROR) {
			mkerror(aexpr->access._struct->loc, expr);
			return;
		};
		if (stype->storage != STORAGE_STRUCT
				&& stype->storage != STORAGE_UNION) {
			error(ctx, aexpr->access._struct->loc, expr,
				"Cannot select field from non-struct, non-union object");
			return;
		}
		expr->access.field = type_get_field(ctx, stype, aexpr->access.field);
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
			type_dereference(ctx, expr->access.tuple->result);
		if (!ttype) {
			error(ctx, aexpr->access.tuple->loc, expr,
				"Cannot dereference nullable pointer for value selection");
			return;
		}
		ttype = type_dealias(ctx, ttype);
		if (ttype->storage == STORAGE_ERROR) {
			mkerror(aexpr->access.tuple->loc, expr);
			return;
		};
		if (ttype->storage != STORAGE_TUPLE) {
			error(ctx, aexpr->access.tuple->loc, expr,
				"Cannot select value from non-tuple object");
			return;
		}
		if (!type_is_integer(ctx, value->result)) {
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
check_expr_alloc_init(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	// alloc(initializer) case
	int ptrflags = 0;
	const struct type *inithint = NULL;
	if (hint) {
		const struct type *htype = type_dealias(ctx, hint);
		switch (htype->storage) {
		case STORAGE_POINTER:
			inithint = htype->pointer.referent;
			// TODO: Describe the use of pointer flags in the spec
			ptrflags = htype->pointer.flags;
			break;
		case STORAGE_SLICE:
			inithint = hint;
			break;
		case STORAGE_TAGGED:
			// TODO
			break;
		default:
			// The user's code is wrong here, but we'll let it fail
			// later.
			break;
		}
	}

	check_expression(ctx, aexpr->alloc.init, expr->alloc.init, inithint);

	const struct type *objtype = expr->alloc.init->result;
	if (type_dealias(ctx, objtype)->storage == STORAGE_ARRAY
			&& type_dealias(ctx, objtype)->array.expandable) {
		const struct type *atype = type_dealias(ctx, objtype);
		if (!inithint) {
			error(ctx, aexpr->loc, expr,
				"Cannot infer expandable array length without type hint");
			return;
		}
		const struct type *htype = type_dealias(ctx, inithint);
		if (htype->storage != STORAGE_ARRAY) {
			error(ctx, aexpr->loc, expr,
				"Cannot assign expandable array from non-array type");
			return;
		}
		assert(htype->array.members == atype->array.members);
		objtype = inithint;
	}
	if (type_is_constant(objtype)) {
		objtype = lower_const(ctx, objtype, inithint);
	} else if (inithint) {
		// XXX: this is dumb, but we're gonna get rid of the const flag
		// anyway so it doesn't matter
		struct type stripped_objtype = *type_dealias(ctx, objtype);
		stripped_objtype.flags &= ~TYPE_CONST;
		stripped_objtype.id = type_hash(&stripped_objtype);
		struct type stripped_inithint = *type_dealias(ctx, inithint);
		stripped_inithint.flags &= ~TYPE_CONST;
		stripped_inithint.id = type_hash(&stripped_inithint);

		if (stripped_objtype.id == stripped_inithint.id) {
			objtype = inithint;
		}
	}
	expr->result = type_store_lookup_pointer(ctx->store, aexpr->loc,
			objtype, ptrflags);
	if (expr->alloc.init->result->size == 0) {
		error(ctx, aexpr->loc, expr,
			"Cannot allocate object with size 0");
		return;
	}
	if (expr->alloc.init->result->size == SIZE_UNDEFINED) {
		error(ctx, aexpr->loc, expr,
			"Cannot allocate object of undefined size");
		return;
	}
}

static void
check_expr_alloc_slice(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	// alloc(init, capacity) case
	check_expression(ctx, aexpr->alloc.init, expr->alloc.init, hint);

	const struct type *objtype = expr->alloc.init->result;
	if (type_dealias(ctx, objtype)->storage == STORAGE_ARRAY) {
		if (type_dealias(ctx, objtype)->array.length == SIZE_UNDEFINED) {
			error(ctx, aexpr->alloc.init->loc, expr,
				"Slice initializer must have defined length");
			return;
		}
	} else if (type_dealias(ctx, objtype)->storage != STORAGE_SLICE) {
		error(ctx, aexpr->alloc.init->loc, expr,
			"Slice initializer must be of slice or array type, not %s",
			type_storage_unparse(type_dealias(ctx, objtype)->storage));
		return;
	}

	const struct type *caphint = &builtin_type_size;
	expr->alloc.cap = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->alloc.cap, expr->alloc.cap, caphint);

	const struct type *captype = expr->alloc.cap->result;
	if (!type_is_assignable(ctx, &builtin_type_size, captype)) {
		error(ctx, aexpr->alloc.cap->loc, expr,
			"Slice capacity must be assignable to size");
		return;
	}
	expr->alloc.cap = lower_implicit_cast(ctx, &builtin_type_size, expr->alloc.cap);

	struct expression cap = {0};
	if (expr->alloc.init->type == EXPR_CONSTANT
			&& expr->alloc.cap->type == EXPR_CONSTANT
			&& eval_expr(ctx, expr->alloc.cap, &cap) == EVAL_OK) {
		uint64_t len = 0;
		for (struct array_constant *c = expr->alloc.init->constant.array;
				c != NULL; c = c->next) {
			len++;
		}
		if (cap.constant.uval < len) {
			error(ctx, aexpr->alloc.cap->loc, expr,
				"Slice capacity cannot be smaller than length of initializer");
			return;
		}
	}

	const struct type *membtype = type_dealias(ctx, objtype)->array.members;
	expr->result = type_store_lookup_slice(ctx->store,
		aexpr->alloc.init->loc, membtype);

	if (objtype->storage == STORAGE_ARRAY
			&& objtype->array.expandable) {
		expr->alloc.kind = ALLOC_WITH_LEN;
	}
}

static void
check_expr_alloc_copy(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	// alloc(init...) case
	check_expression(ctx, aexpr->alloc.init, expr->alloc.init, hint);

	const struct type *result = type_dealias(ctx, expr->alloc.init->result);
	if (result->storage != STORAGE_ARRAY
			&& result->storage != STORAGE_SLICE) {
		error(ctx, aexpr->alloc.init->loc, expr,
			"Slice initializer must be of slice or array type, not %s",
			type_storage_unparse(result->storage));
		return;
	}
	if (hint) {
		const struct type *htype = type_dealias(ctx, hint);
		if (htype->storage != STORAGE_SLICE
				&& htype->storage != STORAGE_TAGGED) {
			error(ctx, aexpr->alloc.init->loc, expr,
				"Hint must be a slice type, not %s",
				type_storage_unparse(htype->storage));
			return;
		}
	}

	check_expression(ctx, aexpr->alloc.init, expr->alloc.init, hint);
	result = type_dealias(ctx, expr->alloc.init->result);
	expr->result = type_store_lookup_slice(ctx->store,
			aexpr->alloc.init->loc, result->array.members);
}

static void
check_expr_alloc(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	assert(aexpr->type == EXPR_ALLOC);
	expr->type = EXPR_ALLOC;
	expr->alloc.init = xcalloc(1, sizeof(struct expression));
	expr->alloc.kind = aexpr->alloc.kind;
	switch (aexpr->alloc.kind) {
	case ALLOC_OBJECT:
		check_expr_alloc_init(ctx, aexpr, expr, hint);
		break;
	case ALLOC_WITH_CAP:
		check_expr_alloc_slice(ctx, aexpr, expr, hint);
		break;
	case ALLOC_COPY:
		check_expr_alloc_copy(ctx, aexpr, expr, hint);
		break;
	case ALLOC_WITH_LEN:
		abort(); // Not determined by parse
	}
}

static void
check_expr_append_insert(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	assert(aexpr->type == EXPR_APPEND || aexpr->type == EXPR_INSERT);
	expr->loc = aexpr->loc;
	expr->type = aexpr->type;
	expr->result = &builtin_type_void;
	expr->append.is_static = aexpr->append.is_static;
	expr->append.is_multi = aexpr->append.is_multi;
	expr->append.object = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->append.object, expr->append.object, NULL);
	if (expr->append.object->result->storage == STORAGE_ERROR) {
		mkerror(aexpr->loc, expr);
		return;
	};
	if (expr->append.object->type != EXPR_ACCESS) {
		error(ctx, aexpr->append.object->loc, expr,
			"Expression must operate on an object");
		return;
	};

	const struct type *sltype;
	const struct type *sltypename;
	const char *exprtype_name;
	switch (expr->type) {
	case EXPR_APPEND:
		sltypename = expr->append.object->result;
		exprtype_name = "append";
		break;
	case EXPR_INSERT:
		assert(expr->append.object->access.type == ACCESS_INDEX);
		sltypename = expr->append.object->access.array->result;
		exprtype_name = "insert";
		break;
	default:
		abort(); // Invariant
	}
	sltype = type_dereference(ctx, sltypename);
	if (!sltype) {
		error(ctx, aexpr->access.tuple->loc, expr,
			"Cannot dereference nullable pointer for %s expression",
			exprtype_name);
		return;
	}
	sltype = type_dealias(ctx, sltype);

	if (sltype->storage != STORAGE_SLICE) {
		char *typename = gen_typename(sltypename);
		error(ctx, aexpr->append.object->loc, expr,
			"%s expression must operate on a slice, but got %s",
			exprtype_name, typename);
		free(typename);
		return;
	}
	if (sltype->flags & TYPE_CONST) {
		error(ctx, aexpr->append.object->loc, expr,
			"expression must operate on a mutable slice");
		return;
	}

	expr->append.value = xcalloc(1, sizeof(struct expression));

	if (!expr->append.is_multi && !aexpr->append.length) {
		check_expression(ctx, aexpr->append.value, expr->append.value,
				sltype->array.members);
		if (!type_is_assignable(ctx, sltype->array.members,
				expr->append.value->result)) {
			error(ctx, aexpr->append.value->loc, expr,
				"Value type must be assignable to object member type");
			return;
		}
		expr->append.value = lower_implicit_cast(ctx, 
			sltype->array.members, expr->append.value);
		return;
	}

	check_expression(ctx, aexpr->append.value, expr->append.value, sltype);
	const struct type *valtype = type_dereference(ctx, expr->append.value->result);
	if (!valtype) {
		error(ctx, aexpr->loc, expr,
			"Cannot dereference nullable pointer for %s expression",
			exprtype_name);
		return;
	}
	valtype = type_dealias(ctx, valtype);
	if (aexpr->append.length) {
		if (valtype->storage != STORAGE_ARRAY
				|| !valtype->array.expandable) {
			error(ctx, aexpr->append.value->loc, expr,
				"Value must be an expandable array in append with length");
			return;
		}
		struct expression *len = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->append.length, len, &builtin_type_size);
		if (!type_is_assignable(ctx, &builtin_type_size, len->result)) {
			error(ctx, aexpr->append.length->loc, expr,
				"Length parameter must be assignable to size");
			return;
		}
		len = lower_implicit_cast(ctx, &builtin_type_size, len);
		expr->append.length = len;
	} else {
		if (valtype->storage != STORAGE_SLICE
				&& valtype->storage != STORAGE_ARRAY) {
			error(ctx, aexpr->append.value->loc, expr,
				"Value must be an array or a slice in multi-valued %s",
				exprtype_name);
			return;
		}
	}
	if (sltype->array.members != valtype->array.members) {
		error(ctx, aexpr->loc, expr,
			"Value member type must match object member type");
		return;
	}
}

static void
check_assert(struct context *ctx,
	struct ast_expression_assert e,
	struct location loc,
	struct expression *expr)
{
	expr->result = &builtin_type_void;
	expr->type = EXPR_ASSERT;

	if (e.cond != NULL) {
		expr->assert.cond = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, e.cond, expr->assert.cond, &builtin_type_bool);
		loc = e.cond->loc;
		if (type_dealias(ctx, expr->assert.cond->result)->storage != STORAGE_BOOL) {
			error(ctx, loc, expr, "Assertion condition must be boolean");
			return;
		}
	} else {
		expr->terminates = !e.is_static;
	}
	if (e.message == NULL) {
		expr->assert.fixed_reason = ABORT_ANON_ASSERTION_FAILED;
	} else {
		expr->assert.message = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, e.message, expr->assert.message, &builtin_type_str);
		if (type_dealias(ctx, expr->assert.message->result)->storage != STORAGE_STRING) {
			error(ctx, e.message->loc, expr,
				"Assertion message must be string");
			return;
		}
	}

	if (e.is_static) {
		expr->type = EXPR_CONSTANT;
		bool cond = false;
		if (expr->assert.cond != NULL) {
			struct expression out = {0}, msgout = {0};
			enum eval_result r =
				eval_expr(ctx, expr->assert.cond, &out);
			if (r != EVAL_OK) {
				error(ctx, e.cond->loc, expr,
					"Unable to evaluate static assertion condition at compile time");
				return;
			}
			if (expr->assert.message) {
				r = eval_expr(ctx, expr->assert.message, &msgout);
				if (r != EVAL_OK) {
					error(ctx, e.message->loc, expr,
						"Unable to evaluate static assertion message at compile time");
					return;
				}
			}
			assert(type_dealias(ctx, out.result)->storage == STORAGE_BOOL);
			cond = out.constant.bval;
		}
		// XXX: Should these abort immediately?
		if (!cond) {
			if (e.message != NULL) {
				error(ctx, loc, expr, "Static assertion failed: %.*s",
					expr->assert.message->constant.string.len,
					expr->assert.message->constant.string.value);
			} else {
				error(ctx, loc, expr, "Static assertion failed");
			}
		}
	}
}

static void
check_expr_assert(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	check_assert(ctx, aexpr->assert, aexpr->loc, expr);
}

static void
check_binarithm_op(struct context *ctx, struct expression *expr,
		enum binarithm_operator op)
{
	const struct type *dealiased = type_dealias(ctx, expr->result);
	switch (op) {
	// Numeric arithmetic
	case BIN_DIV:
	case BIN_MINUS:
	case BIN_PLUS:
	case BIN_TIMES:
		if (!type_is_numeric(ctx, dealiased)) {
			error(ctx, expr->loc, expr,
				"Cannot perform arithmetic on non-numeric %s type",
				type_storage_unparse(dealiased->storage));
		}
		return;
	// Integer artithmetic
	case BIN_BAND:
	case BIN_BOR:
	case BIN_LSHIFT:
	case BIN_MODULO:
	case BIN_RSHIFT:
	case BIN_BXOR:
		if (!type_is_integer(ctx, dealiased)) {
			error(ctx, expr->loc, expr,
				"Cannot perform operation on non-integer %s type",
				type_storage_unparse(dealiased->storage));
		}
		return;
	// Logical arithmetic
	case BIN_LAND:
	case BIN_LOR:
	case BIN_LXOR:
		expr->result = &builtin_type_bool;
		if (dealiased->storage != STORAGE_BOOL) {
			error(ctx, expr->loc, expr,
				"Cannot perform logical arithmetic on non-bool %s type",
				type_storage_unparse(dealiased->storage));
		}
		return;
	case BIN_GREATER:
	case BIN_GREATEREQ:
	case BIN_LESS:
	case BIN_LESSEQ:
		expr->result = &builtin_type_bool;
		if (!type_is_numeric(ctx, dealiased)) {
			error(ctx, expr->loc, expr,
				"Cannot perform comparison on non-numeric %s type",
				type_storage_unparse(dealiased->storage));
		}
		return;
	case BIN_LEQUAL:
	case BIN_NEQUAL:
		expr->result = &builtin_type_bool;
		if (!type_is_numeric(ctx, dealiased) &&
				dealiased->storage != STORAGE_POINTER
				&& dealiased->storage != STORAGE_STRING
				&& dealiased->storage != STORAGE_BOOL
				&& dealiased->storage != STORAGE_RCONST
				&& dealiased->storage != STORAGE_RUNE) {
			error(ctx, expr->loc, expr,
				"Cannot perform equality test on %s dealiased",
				type_storage_unparse(dealiased->storage));
		}
		return;
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
	expr->assign.op = aexpr->assign.op;

	struct expression *object = xcalloc(1, sizeof(struct expression));
	struct expression *value = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->assign.object, object, NULL);
	check_expression(ctx, aexpr->assign.value, value, object->result);

	if (object->type == EXPR_CONSTANT
			&& object->result != &builtin_type_error) {
		error(ctx, aexpr->assign.object->loc, expr,
			"Cannot assign to constant");
		return;
	}
	if (object->result->size == SIZE_UNDEFINED) {
		error(ctx, aexpr->loc, expr,
			"Cannot assign to object with undefined size");
		return;
	}
	if (!type_is_assignable(ctx, object->result, value->result)) {
		char *valtypename = gen_typename(value->result);
		char *objtypename = gen_typename(object->result);
		error(ctx, aexpr->loc, expr,
			"rvalue type (%s) is not assignable to lvalue (%s)",
			valtypename, objtypename);
		free(valtypename);
		free(objtypename);
		return;
	}
	if (expr->assign.op != BIN_LEQUAL) {
		check_binarithm_op(ctx, object, expr->assign.op);
	}

	expr->assign.value = lower_implicit_cast(ctx, object->result, value);
	expr->assign.object = object;
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

	da = type_dealias(store->check_context, da);
	db = type_dealias(store->check_context, db);

	if (da == db) {
		return a->storage == STORAGE_ALIAS ? a : b;
	}

	if (type_is_constant(da) || type_is_constant(db)) {
		return promote_const(store->check_context, a, b);
	}

	if (db->storage == STORAGE_ENUM && da->storage == db->alias.type->storage) {
		return b;
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
	case STORAGE_ENUM:
		if (da->alias.type->storage == db->storage) {
			return a;
		}
		return NULL;
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
		if (!type_is_integer(store->check_context, db)
				|| !type_is_signed(store->check_context, db)
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
		if (da->storage == STORAGE_SIZE && db->storage == STORAGE_UINTPTR) {
			return db;
		}
		if (!type_is_integer(store->check_context, db)
				|| type_is_signed(store->check_context, db)
				|| db->size == da->size) {
			return NULL;
		}
		return da->size > db->size ? a : b;
	case STORAGE_F32:
	case STORAGE_F64:
		if (!type_is_float(store->check_context, db)
				|| db->size == da->size) {
			return NULL;
		}
		return da->size > db->size ? a : b;
	case STORAGE_POINTER:
		if (db->storage == STORAGE_NULL) {
			return a;
		}
		if (db->storage == STORAGE_UINTPTR) {
			return a;
		};
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
		if (db->storage == STORAGE_POINTER
				|| db->storage == STORAGE_UINTPTR) {
			return b;
		}
		return NULL;
	case STORAGE_ERROR:
		return b;
	case STORAGE_UINTPTR:
		if (db->storage == STORAGE_SIZE
				|| db->storage == STORAGE_NULL) {
			return a;
		}
		if (db->storage == STORAGE_POINTER) {
			return b;
		}
		return NULL;
	// Cannot be promoted
	case STORAGE_BOOL:
	case STORAGE_FUNCTION:
	case STORAGE_RUNE:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
	case STORAGE_VOID:
		return NULL;
	// Handled above
	case STORAGE_ALIAS:
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_RCONST:
		assert(0);
	}
	assert(0);
}

static void resolve_enum_field(struct context *ctx,
	struct incomplete_declaration *idel);

static bool
type_has_default(struct context *ctx, const struct type *type)
{
	switch (type->storage) {
	case STORAGE_VOID:
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_BOOL:
	case STORAGE_RUNE:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_INT:
	case STORAGE_SIZE:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_ERROR:
		return true;
	case STORAGE_FUNCTION:
	case STORAGE_TAGGED:
	case STORAGE_VALIST:
		return false;
	case STORAGE_ENUM:
		for (const struct scope_object *obj = type->_enum.values->objects;
				obj != NULL; obj = obj->lnext) {
			if (obj->otype == O_DECL) {
				continue;
			}
			if (obj->otype == O_SCAN) {
				wrap_resolver(ctx, obj, resolve_enum_field);
			}
			assert(obj->otype == O_CONST);
			if (obj->value->constant.uval == 0) {
				return true;
			}
		}
		return false;
	case STORAGE_POINTER:
		return type->pointer.flags & PTR_NULLABLE;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		// TODO: shouldn't be possible to initialize overlapping fields
		// (@offset)
		// See also: https://todo.sr.ht/~sircmpwn/hare/513
		for (struct struct_field *sf = type->struct_union.fields;
				sf != NULL; sf = sf->next) {
			if (!type_has_default(ctx, sf->type)) {
				return false;
			}
		}
		return true;
	case STORAGE_TUPLE:
		for (const struct type_tuple *t = &type->tuple;
				t != NULL; t = t->next) {
			if (!type_has_default(ctx, t->type)) {
				return false;
			}
		}
		return true;
	case STORAGE_ALIAS:
		return type_has_default(ctx, type_dealias(ctx, type));
	case STORAGE_FCONST:
	case STORAGE_ICONST:
	case STORAGE_NULL:
	case STORAGE_RCONST:
		abort(); // unreachable
	}
	abort(); // Unreachable
}

static void
check_expr_binarithm(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_BINARITHM;
	expr->binarithm.op = aexpr->binarithm.op;

	struct expression *lvalue = xcalloc(1, sizeof(struct expression)),
		*rvalue = xcalloc(1, sizeof(struct expression));
	// XXX: Should hints be passed down?
	(void)hint;
	check_expression(ctx, aexpr->binarithm.lvalue, lvalue, NULL);
	check_expression(ctx, aexpr->binarithm.rvalue, rvalue, NULL);

	expr->result = type_promote(ctx->store, lvalue->result, rvalue->result);
	if (expr->result == NULL) {
		char *ltypename = gen_typename(lvalue->result);
		char *rtypename = gen_typename(rvalue->result);
		error(ctx, aexpr->loc, expr,
			"Cannot promote lvalue %s and rvalue %s",
			ltypename, rtypename);
		free(ltypename);
		free(rtypename);
		return;
	}
	expr->binarithm.lvalue = lower_implicit_cast(ctx, expr->result, lvalue);
	expr->binarithm.rvalue = lower_implicit_cast(ctx, expr->result, rvalue);

	check_binarithm_op(ctx, expr, expr->binarithm.op);
}

static void
check_binding_unpack(struct context *ctx,
	const struct type *type,
	const struct ast_expression_binding *abinding,
	struct expression_binding *binding,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	assert(abinding->unpack);
	const struct ast_binding_unpack *cur = abinding->unpack;
	binding->unpack = xcalloc(1, sizeof(struct binding_unpack));
	struct binding_unpack *unpack = binding->unpack;

	struct expression *initializer = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, abinding->initializer, initializer, type);
	if (type_dealias(ctx, initializer->result)->storage != STORAGE_TUPLE) {
		error(ctx, aexpr->loc, expr, "Could not unpack non-tuple type");
		return;
	}

	if (!type) {
		type = type_store_lookup_with_flags(
			ctx->store, initializer->result, abinding->flags);
	}
	type = type_dealias(ctx, type);

	binding->initializer = lower_implicit_cast(ctx, type, initializer);

	if (abinding->is_static) {
		struct expression *value = xcalloc(1, sizeof(struct expression));
		enum eval_result r = eval_expr(ctx, binding->initializer, value);
		if (r != EVAL_OK) {
			error(ctx, abinding->initializer->loc,
				expr,
				"Unable to evaluate static initializer at compile time");
			return;
		}
		// TODO: Free initializer
		binding->initializer = value;
		assert(binding->initializer->type == EXPR_CONSTANT);
	}

	if (type->storage != STORAGE_TUPLE) {
		error(ctx, abinding->initializer->loc, expr,
			"Unable to unpack tuple with non-tuple type specifier");
		return;
	}
	const struct type_tuple *type_tuple = &type->tuple;
	bool found_binding = false;
	while (cur && type_tuple) {
		if (type_tuple->type->storage == STORAGE_NULL) {
			error(ctx, aexpr->loc, expr,
				"Null is not a valid type for a binding");
			return;
		}

		if (cur->name) {
			struct identifier ident = {
				.name = cur->name,
			};

			if (abinding->is_static) {
				struct identifier gen = {0};

				// Generate a static declaration identifier
				gen.name = gen_name(&ctx->id, "static.%d");

				unpack->object = scope_insert(
					ctx->scope, O_DECL, &gen, &ident,
					type_tuple->type, NULL);
			} else {
				unpack->object = scope_insert(
					ctx->scope, O_BIND, &ident, &ident,
					type_tuple->type, NULL);
			}

			unpack->offset = type_tuple->offset;

			found_binding = true;
		}

		cur = cur->next;
		type_tuple = type_tuple->next;

		if (cur && found_binding && cur->name) {
			unpack->next = xcalloc(1, sizeof(struct binding_unpack));
			unpack = unpack->next;
		}
	}

	if (!found_binding) {
		error(ctx, aexpr->loc, expr,
			"Must have at least one non-underscore value when unpacking tuples");
		return;
	}

	if (type_tuple) {
		error(ctx, aexpr->loc, expr,
			"Fewer bindings than tuple elements were provided when unpacking");
		return;
	}
	if (cur) {
		error(ctx, aexpr->loc, expr,
			"More bindings than tuple elements were provided when unpacking");
		return;
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

		if (abinding->unpack) {
			check_binding_unpack(ctx, type, abinding, binding,
				aexpr, expr);
			goto done;
		}

		struct expression *initializer =
			xcalloc(1, sizeof(struct expression));
		check_expression(ctx, abinding->initializer, initializer, type);

		if (abinding->type
				&& abinding->type->storage == STORAGE_ARRAY
				&& abinding->type->array.contextual) {
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

		if (!type) {
			type = type_store_lookup_with_flags(ctx->store,
				initializer->result, abinding->flags);
		}

		struct identifier ident = {
			.name = abinding->name,
		};
		if (abinding->is_static) {
			// Generate a static declaration identifier
			struct identifier gen = {0};
			gen.name = gen_name(&ctx->id, "static.%d");
			binding->object = scope_insert(ctx->scope,
				O_DECL, &gen, &ident, type, NULL);
		} else {
			binding->object = scope_insert(ctx->scope,
				O_BIND, &ident, &ident, type, NULL);
		}

		if (type->storage == STORAGE_NULL) {
			error(ctx, aexpr->loc, expr,
				"Null is not a valid type for a binding");
			return;
		}
		if (!type_is_assignable(ctx, type, initializer->result)) {
			error(ctx, aexpr->loc, expr,
				"Initializer is not assignable to binding type");
			return;
		}
		// XXX: Can we avoid this?
		type = lower_const(ctx, type, NULL);
		if (type->size == 0 || type->size == SIZE_UNDEFINED) {
			error(ctx, aexpr->loc, expr,
				"Cannot create binding for type of zero or undefined size");
			return;
		}
		binding->initializer = lower_implicit_cast(ctx, type, initializer);

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

done:
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
	const struct type *hint = type_store_lookup_array(ctx->store,
			val.loc, type, SIZE_UNDEFINED, false);
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

	const struct type *fntype = type_dereference(ctx, lvalue->result);
	if (!fntype) {
		error(ctx, aexpr->loc, expr,
			"Cannot dereference nullable pointer type for function call");
		return;
	}
	fntype = type_dealias(ctx, fntype);
	if (fntype->storage == STORAGE_ERROR) {
		mkerror(aexpr->loc, expr);
		return;
	};
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
	while ((param || fntype->func.variadism == VARIADISM_C) && aarg) {
		arg = *next = xcalloc(1, sizeof(struct call_argument));
		arg->value = xcalloc(1, sizeof(struct expression));

		if (param && !param->next
				&& fntype->func.variadism == VARIADISM_HARE
				&& !aarg->variadic) {
			lower_vaargs(ctx, aarg, arg->value,
				param->type->array.members);
			arg->value = lower_implicit_cast(ctx, param->type, arg->value);
			param = NULL;
			aarg = NULL;
			break;
		}

		const struct type *ptype = NULL;
		if (param) {
			ptype = param->type;
		}
		check_expression(ctx, aarg->value, arg->value, ptype);

		if (param) {
			if (!type_is_assignable(ctx, ptype, arg->value->result)) {
				char *argtypename = gen_typename(arg->value->result);
				char *paramtypename = gen_typename(param->type);
				error(ctx, aarg->value->loc, expr,
					"Argument type %s is not assignable to parameter type %s",
					argtypename, paramtypename);
				free(argtypename);
				free(paramtypename);
				return;
			}
			arg->value = lower_implicit_cast(ctx, ptype, arg->value);
		}

		aarg = aarg->next;
		next = &arg->next;
		if (param) {
			param = param->next;
		}
	}

	if (param && !param->next && fntype->func.variadism == VARIADISM_HARE) {
		// No variadic arguments, lower to empty slice
		arg = *next = xcalloc(1, sizeof(struct call_argument));
		arg->value = xcalloc(1, sizeof(struct expression));
		lower_vaargs(ctx, NULL, arg->value,
			param->type->array.members);
		arg->value = lower_implicit_cast(ctx, param->type, arg->value);
		param = param->next;
	}

	if (aarg && fntype->func.variadism != VARIADISM_C) {
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
	check_expression(ctx, aexpr->cast.value, value,
			secondary == &builtin_type_void ? NULL : secondary);
	if (value->terminates) {
		error(ctx, aexpr->cast.value->loc, expr,
			"Cast must not operate on terminating expression");
		return;
	}

	const struct type *primary = type_dealias(ctx, expr->cast.value->result);
	if (primary->storage == STORAGE_ERROR) {
		mkerror(aexpr->cast.value->loc, expr);
		return;
	};
	switch (aexpr->cast.kind) {
	case C_ASSERTION:
	case C_TEST:
		if (primary->storage == STORAGE_POINTER) {
			if (!(primary->pointer.flags & PTR_NULLABLE)) {
				error(ctx, aexpr->cast.value->loc, expr,
					"Expected a tagged union type or "
					"a nullable pointer");
				return;
			}
			if (secondary->storage != STORAGE_NULL
					&& (secondary->storage != STORAGE_POINTER
					|| primary->pointer.referent
						!= secondary->pointer.referent
					|| (secondary->pointer.flags & PTR_NULLABLE))) {
				error(ctx, aexpr->cast.type->loc, expr,
					"Can only type assert nullable pointer into non-nullable pointer of the same type or null");
				return;
			}
			break;
		}
		if (primary->storage != STORAGE_TAGGED) {
			error(ctx, aexpr->cast.value->loc, expr,
				"Expected a tagged union type or "
				"a nullable pointer");
			return;
		}
		// secondary type must be a strict subset or a
		// member of the primary type
		if (!((tagged_subset_compat(ctx, primary, secondary)
				|| tagged_select_subtype(ctx, primary, secondary, true))
				&& !tagged_subset_compat(ctx, secondary, primary))) {
			error(ctx, aexpr->cast.type->loc, expr,
				"Type is not a valid member of "
				"the tagged union type");
			return;
		}
		break;
	case C_CAST:;
		const struct type *intermediary =
			type_is_castable(ctx, secondary, value->result);
		if (intermediary == NULL) {
			char *primarytypename = gen_typename(value->result);
			char *secondarytypename = gen_typename(secondary);
			error(ctx, aexpr->cast.type->loc, expr,
				"Invalid cast from %s to %s",
				primarytypename, secondarytypename);
			free(primarytypename);
			free(secondarytypename);
			return;
		}
		// intermediary type is required when casting to tagged union
		// whose member is an alias of primary type, since gen.c asserts
		// that the primary type is a direct member of the tagged union.
		// The value is first cast to an intermediary type which is a
		// direct member of the tagged union, before being cast to the
		// tagged union itself.
		expr->cast.value = xcalloc(1, sizeof(struct expression));
		expr->cast.value->type = EXPR_CAST;
		expr->cast.value->result = intermediary;
		expr->cast.value->cast.kind = C_CAST;
		expr->cast.value->cast.value = value;
		expr->cast.value->cast.secondary = intermediary;
		if (value->result->storage == STORAGE_RCONST) {
			uint32_t max = 0;
			switch (secondary->storage) {
			case STORAGE_RUNE:
			case STORAGE_U64:
			case STORAGE_I64:
			case STORAGE_U32:
			case STORAGE_UINT:
			case STORAGE_SIZE:
			case STORAGE_UINTPTR:
				break;
			case STORAGE_I32:
				max = INT32_MAX;
				break;
			case STORAGE_U16:
				max = UINT16_MAX;
				break;
			case STORAGE_I16:
				max = INT16_MAX;
				break;
			case STORAGE_U8:
				max = UINT8_MAX;
				break;
			case STORAGE_I8:
				max = INT8_MAX;
				break;
			default:
				assert(0); // Invariant
			}

			if (max != 0 && value->constant.rune > max) {
				char *typename = gen_typename(secondary);
				error(ctx, aexpr->cast.type->loc, expr,
					"Rune does not fit in %s",
					typename);
				free(typename);
				return;
			}
		}
		break;
	}
	expr->result = aexpr->cast.kind == C_TEST? &builtin_type_bool : secondary;
}

static void
check_expr_array(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	size_t len = 0;
	bool expand = false;
	struct ast_array_constant *item = aexpr->constant.array;
	struct array_constant *cur, **next = &expr->constant.array;
	const struct type *type = NULL;
	if (hint) {
		hint = type_dealias(ctx, hint);

		size_t narray = 0;
		switch (hint->storage) {
		case STORAGE_ARRAY:
		case STORAGE_SLICE:
			type = hint->array.members;
			break;
		case STORAGE_TAGGED:
			for (const struct type_tagged_union *tu = &hint->tagged;
					tu; tu = tu->next) {
				const struct type *t = type_dealias(ctx, tu->type);
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
			break;
		default:
			hint = NULL;
			break;
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
			if (!hint) {
				// The promote_const in
				// check_expression_constant might've caused the
				// type to change out from under our feet
				type = expr->constant.array->value->result;
			}
			if (!type_is_assignable(ctx, type, value->result)) {
				char *typename1 = gen_typename(type);
				char *typename2 = gen_typename(value->result);
				error(ctx, item->value->loc, expr,
					"Array members must be of a uniform type, previously seen %s, but now see %s",
					typename1, typename2);
				free(typename1);
				free(typename2);
				return;
			}
			if (!hint) {
				// Ditto
				type = expr->constant.array->value->result;
			}
			cur->value = lower_implicit_cast(ctx, type, cur->value);
		}

		if (item->expand) {
			expand = true;
			assert(!item->next);
		}

		item = item->next;
		next = &cur->next;
		++len;
	}

	if (type == NULL) {
		error(ctx, aexpr->loc, expr, "Cannot infer array type from context, try casting it to the desired type");
		return;
	}
	expr->result = type_store_lookup_array(ctx->store, aexpr->loc,
			type, len, expand);
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
		expr->compound.label = xstrdup(aexpr->compound.label);
		scope->label = xstrdup(aexpr->compound.label);
	}

	struct expressions *list = &expr->compound.exprs;
	struct expressions **next = &list->next;

	const struct ast_expression_list *alist = &aexpr->compound.list;
	struct expression *lexpr = NULL;
	while (alist) {
		lexpr = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, alist->expr, lexpr, NULL);
		if (type_has_error(ctx, lexpr->result)) {
			error(ctx, alist->expr->loc, lexpr,
				"Cannot ignore error here");
		}
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

	expr->terminates = false;
	if (lexpr->terminates && scope->yields == NULL) {
		expr->terminates = true;
		if (lexpr->type == EXPR_YIELD) {
			const char *llabel = lexpr->control.label;
			if (!llabel || (scope->label
					&& strcmp(llabel, scope->label) == 0)) {
				expr->terminates = false;
			}
		}
	}
	if (!lexpr->terminates) {
		// Add implicit `yield void` if control reaches end of compound
		// expression.
		struct type_tagged_union *result =
			xcalloc(1, sizeof(struct type_tagged_union));
		result->type = &builtin_type_void;
		result->next = scope->results;
		scope->results = result;

		list->next = xcalloc(1, sizeof(struct expressions));
		struct ast_expression *yexpr = xcalloc(1, sizeof(struct ast_expression));
		yexpr->type = EXPR_YIELD;
		lexpr = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, yexpr, lexpr, NULL);
		list->next->expr = lexpr;
	}
	expr->result = type_store_reduce_result(ctx->store, aexpr->loc,
			scope->results);

	for (struct yield *yield = scope->yields; yield;) {
		struct expression *lowered = lower_implicit_cast(ctx, 
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
	enum type_storage storage = aexpr->constant.storage;
	expr->result = builtin_type_for_storage(storage, false);
	if (storage == STORAGE_ICONST || storage == STORAGE_FCONST
			|| storage == STORAGE_RCONST) {
		expr->result = type_create_const(storage,
			aexpr->constant.ival, aexpr->constant.ival);
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
	case STORAGE_RCONST:
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
	case STORAGE_ENUM:
	case STORAGE_ERROR:
	case STORAGE_UINTPTR:
	case STORAGE_ALIAS:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_RUNE:
	case STORAGE_SLICE:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
	case STORAGE_VALIST:
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
	scope_push(&ctx->scope, SCOPE_DEFER);
	check_expression(ctx, aexpr->defer.deferred, expr->defer.deferred, NULL);
	if (type_has_error(ctx, expr->defer.deferred->result)) {
		error(ctx, aexpr->defer.deferred->loc, expr->defer.deferred,
			"Cannot ignore error here");
	}
	ctx->deferring = false;
	scope_pop(&ctx->scope);
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
	otype = type_dereference(ctx, otype);
	if (!otype) {
		error(ctx, aexpr->loc, expr,
			"Cannot dereference nullable pointer for delete expression");
		return;
	}
	otype = type_dealias(ctx, otype);
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
	struct scope *defer_scope = scope_lookup_ancestor(
		ctx->scope, SCOPE_DEFER, NULL);
	if (defer_scope) {
		defer_scope = scope_lookup_ancestor(
			defer_scope, want, aexpr->control.label);
		if (scope == defer_scope) {
			error(ctx, aexpr->loc, expr,
				"Cannot jump out of defer expression");
			return;
		}
	}
	expr->control.scope = scope;

	if (expr->type != EXPR_YIELD) {
		return;
	}

	expr->control.value = xcalloc(1, sizeof(struct expression));
	if (aexpr->control.value) {
		check_expression(ctx, aexpr->control.value,
			expr->control.value, scope->hint);
	} else {
		expr->control.value->type = EXPR_CONSTANT;
		expr->control.value->result = &builtin_type_void;
	}

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
		if (type_has_error(ctx, bindings->result)) {
			error(ctx, aexpr->_for.bindings->loc, bindings,
				"Cannot ignore error here");
		}
		expr->_for.bindings = bindings;
	}

	cond = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_for.cond, cond, &builtin_type_bool);
	expr->_for.cond = cond;
	if (type_dealias(ctx, cond->result)->storage != STORAGE_BOOL) {
		error(ctx, aexpr->_for.cond->loc, expr,
			"Expected for condition to be boolean");
		return;
	}

	if (aexpr->_for.afterthought) {
		afterthought = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->_for.afterthought, afterthought, &builtin_type_void);
		if (type_has_error(ctx, afterthought->result)) {
			error(ctx, aexpr->_for.afterthought->loc, afterthought,
				"Cannot ignore error here");
		}
		expr->_for.afterthought = afterthought;
	}

	body = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->_for.body, body, NULL);
	if (type_has_error(ctx, body->result)) {
		error(ctx, aexpr->_for.body->loc, body,
			"Cannot ignore error here");
	}
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
	expr->free.expr = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->free.expr, expr->free.expr, NULL);
	enum type_storage storage = type_dealias(ctx, expr->free.expr->result)->storage;
	if (storage == STORAGE_ERROR) {
		mkerror(aexpr->loc, expr);
		return;
	};
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
		} else if (hint && type_is_assignable(ctx, hint, true_branch->result)
				&& type_is_assignable(ctx, hint, false_branch->result)) {
			expr->result = hint;
		}
	} else {
		expr->terminates = false;
		if (hint && type_is_assignable(ctx, hint, true_branch->result)
				&& type_is_assignable(ctx, hint, &builtin_type_void)) {
			expr->result = hint;
		}
	}
	if (expr->result == NULL) {
		struct type_tagged_union _tags = {
			.type = false_branch ? false_branch->result : &builtin_type_void,
			.next = NULL,
		}, tags = {
			.type = true_branch->result,
			.next = &_tags,
		};
		expr->result = type_store_reduce_result(ctx->store, aexpr->loc, &tags);
		if (expr->result == NULL) {
			error(ctx, aexpr->loc, expr,
				"Invalid result type (dangling or ambiguous null)");
			return;
		}
	}
	true_branch = lower_implicit_cast(ctx, expr->result, true_branch);
	if (false_branch != NULL) {
		false_branch = lower_implicit_cast(ctx, expr->result, false_branch);
	}

	if (cond->result->storage == STORAGE_ERROR) {
		mkerror(aexpr->match.value->loc, expr);
		return;
	}
	if (type_dealias(ctx, cond->result)->storage != STORAGE_BOOL) {
		error(ctx, aexpr->_if.cond->loc, expr,
			"Expected if condition to be boolean");
		return;
	}

	expr->_if.cond = cond;
	expr->_if.true_branch = true_branch;
	expr->_if.false_branch = false_branch;
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

	const struct type *type = type_dealias(ctx, value->result);
	if (type->storage == STORAGE_ERROR) {
		mkerror(aexpr->match.value->loc, expr);
		return;
	}
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
				if (!type_is_assignable(ctx, type, ctype)) {
					error(ctx, acase->type->loc, expr,
						"Invalid type for match case (match is not assignable to this type)");
					return;
				}
			}
		}

		if (acase->name) {
			assert(ctype);
			if (ctype->size == 0 || ctype->size == SIZE_UNDEFINED) {
				error(ctx, acase->type->loc, expr,
					"Cannot create binding for type of zero or undefined size");
				return;
			}
			if (ctype->storage == STORAGE_NULL) {
				error(ctx, aexpr->loc, expr,
					"Null is not a valid type for a binding");
				return;
			}
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

		// Lower to compound
		// TODO: This should probably be done in a more first-class way
		struct ast_expression compound = {
			.type = EXPR_COMPOUND,
			.compound = {
				.list = acase->exprs,
			},
		};
		check_expression(ctx, &compound, _case->value, hint);

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
				ctx->store, aexpr->loc, &result_type);
			if (expr->result == NULL) {
				error(ctx, aexpr->loc, expr,
					"Invalid result type (dangling or ambiguous null)");
				return;
			}
		}

		struct match_case *_case = expr->match.cases;
		struct ast_match_case *acase = aexpr->match.cases;
		while (_case) {
			if (!_case->value->terminates && !type_is_assignable(ctx, 
					expr->result, _case->value->result)) {
				error(ctx, acase->exprs.expr->loc, expr,
					"Match case is not assignable to result type");
				return;
			}
			_case->value = lower_implicit_cast(ctx, 
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
			type_dereference(ctx, expr->measure.value->result);
		if (!atype) {
			error(ctx, aexpr->access.array->loc, expr,
				"Cannot dereference nullable pointer for len");
			return;
		}
		enum type_storage vstor = type_dealias(ctx, atype)->storage;
		bool valid = vstor == STORAGE_ARRAY || vstor == STORAGE_SLICE
			|| vstor == STORAGE_STRING || vstor == STORAGE_ERROR;
		if (!valid) {
			char *typename = gen_typename(expr->measure.value->result);
			error(ctx, aexpr->measure.value->loc, expr,
				"len argument must be of an array, slice, or str type, but got %s",
				typename);
			free(typename);
			return;
		}
		if (atype->size == SIZE_UNDEFINED) {
			error(ctx, aexpr->measure.value->loc, expr,
				"Cannot take length of array type with undefined length");
			return;
		}
		break;
	case M_ALIGN:
		expr->measure.dimensions = type_store_lookup_dimensions(
			ctx->store, aexpr->measure.type);
		if (expr->measure.dimensions.align == ALIGN_UNDEFINED) {
			error(ctx, aexpr->measure.value->loc, expr,
				"Cannot take alignment of a type with undefined alignment");
			return;
		}
		if (expr->measure.dimensions.size == 0) {
			error(ctx, aexpr->measure.value->loc, expr,
				"Cannot take alignment of a type of size 0");
			return;
		}
		break;
	case M_SIZE:
		expr->measure.dimensions = type_store_lookup_dimensions(
			ctx->store, aexpr->measure.type);
		if (expr->measure.dimensions.size == SIZE_UNDEFINED) {
			error(ctx, aexpr->measure.value->loc, expr,
				"Cannot take size of a type with undefined size");
			return;
		}
		break;
	case M_OFFSET:
		if (aexpr->measure.value->type != EXPR_ACCESS) {
			error(ctx, aexpr->measure.value->loc, expr,
				"offset argument must be a field or tuple access");
			return;
		}
		if (aexpr->measure.value->access.type != ACCESS_FIELD
				&& aexpr->measure.value->access.type != ACCESS_TUPLE) {
			error(ctx, aexpr->measure.value->loc, expr,
				"offset argument must be a field or tuple access");
			return;
		}
		expr->measure.value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->measure.value,
			expr->measure.value, NULL);
		break;
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
	if (intype->storage == STORAGE_ERROR) {
		mkerror(aexpr->loc, expr);
		return;
	};
	if (type_dealias(ctx, intype)->storage != STORAGE_TAGGED) {
		char *typename = gen_typename(intype);
		error(ctx, aexpr->loc, expr,
			"Cannot use error propagation on non-tagged type %s",
			typename);
		free(typename);
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

	const struct type_tagged_union *intu = &type_dealias(ctx, intype)->tagged;
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
			ctx->store, aexpr->loc, &return_tagged);
	} else {
		return_type = return_tagged.type;
	}

	const struct type *result_type;
	if (!result_tagged.type) {
		result_type = &builtin_type_void;
	} else if (result_tagged.next) {
		result_type = type_store_lookup_tagged(
			ctx->store, aexpr->loc, &result_tagged);
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

	ok_name.name = gen_name(&ctx->id, "ok.%d");
	err_name.name = gen_name(&ctx->id, "err.%d");
	const struct scope_object *ok_obj = NULL, *err_obj = NULL;
	if (result_type->size != 0 && result_type->size != SIZE_UNDEFINED) {
		ok_obj = scope_insert(scope, O_BIND, &ok_name,
			&ok_name, result_type, NULL);
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

	case_err->value = xcalloc(1, sizeof(struct expression));

	if (aexpr->propagate.abort) {
		case_err->value->loc = expr->loc;
		case_err->value->type = EXPR_ASSERT;
		case_err->value->assert = (struct expression_assert){
			.cond = NULL,
			.message = NULL,
			.fixed_reason = ABORT_PROPAGATE_ERROR_OCCURRED,
		};
	} else {
		if (return_type->size != 0 && return_type->size != SIZE_UNDEFINED) {
			err_obj = scope_insert(scope, O_BIND, &err_name,
				&err_name, return_type, NULL);
		}
		case_err->type = return_type;
		case_err->object = err_obj;
		if (!type_is_assignable(ctx, ctx->fntype->func.result, return_type)) {
			char *res = gen_typename(ctx->fntype->func.result);
			char *ret = gen_typename(return_type);
			error(ctx, aexpr->loc, expr,
				"Error type %s is not assignable to function result type %s",
				ret, res);
			free(res);
			free(ret);
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
		case_err->value->_return.value = lower_implicit_cast(ctx, 
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
	if (ctx->fntype == NULL) {
		error(ctx, aexpr->loc, expr, "Cannot return outside a function body");
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

	if (!type_is_assignable(ctx, ctx->fntype->func.result, rval->result)) {
		char *rettypename = gen_typename(rval->result);
		char *fntypename = gen_typename(ctx->fntype->func.result);
		error(ctx, aexpr->loc, expr,
			"Return value %s is not assignable to function result type %s",
			rettypename, fntypename);
		free(rettypename);
		free(fntypename);
		return;
	}
	if (ctx->fntype->func.result != rval->result) {
		rval = lower_implicit_cast(ctx, 
			ctx->fntype->func.result, rval);
	}
	expr->_return.value = rval;
}

static void
slice_bounds_check(struct context *ctx, struct expression *expr)
{
	const struct type *atype = type_dereference(ctx, expr->slice.object->result);
	const struct type *dtype = type_dealias(ctx, atype);

	struct expression *start = NULL, *end = NULL;

	if (expr->slice.end != NULL) {
		end = xcalloc(1, sizeof(struct expression));
		enum eval_result r = eval_expr(ctx, expr->slice.end, end);
		if (r != EVAL_OK) {
			free(end);
			return;
		}

		if (dtype->storage == STORAGE_ARRAY
				&& dtype->array.length != SIZE_UNDEFINED) {
			if (end->constant.uval > dtype->array.length) {
				error(ctx, expr->loc, expr,
					"End index must not be greater than array length");
				free(end);
				return;
			}
		}
	} else {
		if (dtype->storage != STORAGE_ARRAY) {
			return;
		}
		assert(dtype->array.length != SIZE_UNDEFINED);
	}

	if (expr->slice.start == NULL) {
		if (end) free(end);
		return;
	}
	start = xcalloc(1, sizeof(struct expression));
	enum eval_result r = eval_expr(ctx, expr->slice.start, start);
	if (r != EVAL_OK) {
		free(start);
		if (end) free(end);
		return;
	}

	if (dtype->storage == STORAGE_ARRAY
			&& dtype->array.length != SIZE_UNDEFINED) {
		if (start->constant.uval > dtype->array.length) {
			error(ctx, expr->loc, expr,
				"Start index must not be greater than array length");
			free(start);
			if (end) free(end);
			return;
		}

		expr->slice.bounds_checked = true;
	}

	if (end != NULL) {
		if (start->constant.uval > end->constant.uval) {
			error(ctx, expr->loc, expr,
				"Start index must not be greater than end index");
		}
		free(end);
	}
	free(start);
	return;
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
		type_dereference(ctx, expr->slice.object->result);
	if (!atype) {
		error(ctx, aexpr->slice.object->loc, expr,
			"Cannot dereference nullable pointer for slicing");
		return;
	}
	const struct type *dtype = type_dealias(ctx, atype);
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
		itype = type_dealias(ctx, expr->slice.start->result);
		if (!type_is_integer(ctx, itype)) {
			error(ctx, aexpr->slice.start->loc, expr,
				"Cannot use non-integer %s type as slicing operand",
				type_storage_unparse(itype->storage));
			return;
		}
		expr->slice.start = lower_implicit_cast(ctx, 
			&builtin_type_size, expr->slice.start);
	}

	if (aexpr->slice.end) {
		expr->slice.end = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->slice.end, expr->slice.end, &builtin_type_size);
		itype = type_dealias(ctx, expr->slice.end->result);
		if (!type_is_integer(ctx, itype)) {
			error(ctx, aexpr->slice.end->loc, expr,
				"Cannot use non-integer %s type as slicing operand",
				type_storage_unparse(itype->storage));
			return;
		}
		expr->slice.end = lower_implicit_cast(ctx, 
			&builtin_type_size, expr->slice.end);
	} else if (dtype->storage == STORAGE_ARRAY
			&& dtype->array.length == SIZE_UNDEFINED) {
		error(ctx, aexpr->loc, expr,
			"Must have end index on array of undefined length");
		return;
	}

	slice_bounds_check(ctx, expr);

	if (dtype->storage == STORAGE_SLICE) {
		expr->result = atype;
	} else {
		expr->result = type_store_lookup_slice(ctx->store, aexpr->loc,
			dtype->array.members);
	}
}

static void
check_struct_exhaustive(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *stype)
{
	stype = type_dealias(ctx, stype);
	if (stype->storage == STORAGE_UNION) {
		return;
	}
	assert(stype->storage == STORAGE_STRUCT);
	struct struct_field *sf = stype->struct_union.fields;
	struct ast_field_value *af = aexpr->_struct.fields;

	// XXX: O(n^2)?
	while (sf) {
		bool found = false;
		for (struct ast_field_value *f = af; f;
				f = f->next) {
			if (!sf->name) {
				check_struct_exhaustive(ctx, aexpr, expr,
					sf->type);
				found = true;
				continue;
			}
			if (strcmp(f->name, sf->name) == 0) {
				if (found) {
					error(ctx, aexpr->loc, expr,
						"Field '%s' is initialized multiple times",
						sf->name);
				}
				found = true;
			}
		}

		if (!found && (!aexpr->_struct.autofill
					|| !type_has_default(ctx, sf->type))) {
			error(ctx, aexpr->loc, expr,
				"Field '%s' is uninitialized",
				sf->name);
		}

		sf = sf->next;
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
		// resolve the unknown type
		wrap_resolver(ctx, obj, resolve_type);
		if (!obj) {
			error(ctx, aexpr->loc, expr,
				"Unknown type alias");
			return;
		}
		assert(obj->otype == O_TYPE);
		stype = obj->type;
		enum type_storage storage = type_dealias(ctx, stype)->storage;
		if (storage != STORAGE_STRUCT && storage != STORAGE_UNION) {
			error(ctx, aexpr->loc, expr,
				"Object named is not a struct or union type");
			return;
		}
	}

	struct ast_type satype = {
		.storage = STORAGE_STRUCT,
		.flags = TYPE_CONST,
	};
	struct ast_struct_union_field *tfield = &satype.struct_union.fields;
	struct ast_struct_union_field **tnext = &tfield->next;
	struct expr_struct_field *sexpr, **snext = &expr->_struct.fields;
	expr->_struct.autofill = aexpr->_struct.autofill;
	if (stype == NULL && expr->_struct.autofill) {
		error(ctx, aexpr->loc, expr,
				"Autofill is only permitted for named struct initializers");
		return;
	}

	struct ast_field_value *afield = aexpr->_struct.fields;
	while (afield) {
		const struct type *ftype;
		*snext = sexpr = xcalloc(1, sizeof(struct expr_struct_field));
		snext = &sexpr->next;
		sexpr->value = xcalloc(1, sizeof(struct expression));
		if (!stype) {
			assert(afield->name); // TODO
			if (!afield->type) {
				error(ctx, aexpr->loc, expr,
					"Unnamed struct must specify field type");
				return;
			}
			tfield->name = afield->name;
			tfield->type = afield->type;
			ftype = type_store_lookup_atype(ctx->store, tfield->type);
			check_expression(ctx, afield->initializer,
				sexpr->value, ftype);
			if (afield->next) {
				*tnext = tfield = xcalloc(
					1, sizeof(struct ast_struct_union_type));
				tnext = &tfield->next;
			}
		} else {
			if (!afield->name) {
				error(ctx, afield->initializer->loc, expr,
					"Cannot embed a struct literal into "
					"a named struct literal");
				return;
			}
			sexpr->field = type_get_field(ctx, type_dealias(ctx, stype),
					afield->name);
			if (!sexpr->field) {
				error(ctx, afield->initializer->loc, expr,
					"No field by this name exists for this type");
				return;
			}
			ftype = sexpr->field->type;
			check_expression(ctx, afield->initializer,
					sexpr->value, ftype);

			if (!type_is_assignable(ctx, sexpr->field->type, sexpr->value->result)) {
				error(ctx, afield->initializer->loc, expr,
					"Initializer is not assignable to struct field");
				return;
			}
			sexpr->value = lower_implicit_cast(ctx, 
				sexpr->field->type, sexpr->value);
		}

		afield = afield->next;
	}

	if (stype) {
		expr->result = stype;
		check_struct_exhaustive(ctx, aexpr, expr, stype);
	} else {
		expr->result = type_store_lookup_atype(ctx->store, &satype);

		tfield = &satype.struct_union.fields;
		sexpr = expr->_struct.fields;
		while (tfield) {
			const struct struct_field *field = type_get_field(ctx, 
				expr->result, tfield->name);
			if (!field) {
				// TODO: Use more specific error location
				error(ctx, aexpr->loc, expr,
					"No field by this name exists for this type");
				return;
			}
			if (!type_is_assignable(ctx, field->type, sexpr->value->result)) {
				error(ctx, aexpr->loc, expr,
					"Cannot initialize struct field '%s' from value of this type",
					field->name);
				return;
			}
			sexpr->field = field;
			sexpr->value = lower_implicit_cast(ctx, field->type, sexpr->value);

			struct ast_struct_union_field *next = tfield->next;
			if (tfield != &satype.struct_union.fields) {
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
	const struct type *type = type_dealias(ctx, value->result);
	expr->_switch.value = value;
	if (!type_is_numeric(ctx, type)
			&& type_dealias(ctx, type)->storage != STORAGE_POINTER
			&& type_dealias(ctx, type)->storage != STORAGE_STRING
			&& type_dealias(ctx, type)->storage != STORAGE_BOOL
			&& type_dealias(ctx, type)->storage != STORAGE_RCONST
			&& type_dealias(ctx, type)->storage != STORAGE_RUNE) {
		error(ctx, aexpr->loc, expr,
			"Cannot switch on %s type",
			type_storage_unparse(type_dealias(ctx, type)->storage));
		return;
	}

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
			if (!type_is_assignable(ctx, type_dealias(ctx, type),
					type_dealias(ctx, value->result))) {
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

		// Lower to compound
		// TODO: This should probably be done in a more first-class way
		struct ast_expression compound = {
			.type = EXPR_COMPOUND,
			.compound = {
				.list = acase->exprs,
			},
		};
		check_expression(ctx, &compound, _case->value, hint);

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
				ctx->store, aexpr->loc, &result_type);
			if (expr->result == NULL) {
				error(ctx, aexpr->loc, expr,
					"Invalid result type (dangling or ambiguous null)");
				return;
			}
		}

		struct switch_case *_case = expr->_switch.cases;
		struct ast_switch_case *acase = aexpr->_switch.cases;
		while (_case) {
			if (!_case->value->terminates && !type_is_assignable(ctx, 
					expr->result, _case->value->result)) {
				error(ctx, acase->exprs.expr->loc, expr,
					"Switch case is not assignable to result type");
				return;
			}
			_case->value = lower_implicit_cast(ctx, 
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
	if (hint && type_dealias(ctx, hint)->storage == STORAGE_TUPLE) {
		ttuple = &type_dealias(ctx, hint)->tuple;
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

	if (hint && type_dealias(ctx, hint)->storage == STORAGE_TUPLE) {
		expr->result = hint;
	} else if (hint && type_dealias(ctx, hint)->storage == STORAGE_TAGGED) {
		for (const struct type_tagged_union *tu =
				&type_dealias(ctx, hint)->tagged;
				tu; tu = tu->next) {
			if (type_dealias(ctx, tu->type)->storage != STORAGE_TUPLE) {
				continue;
			}
			const struct type_tuple *ttuple =
				&type_dealias(ctx, tu->type)->tuple;
			const struct expression_tuple *etuple = &expr->tuple;
			bool valid = true;
			while (etuple) {
				if (!ttuple || !type_is_assignable(ctx, ttuple->type,
						etuple->value->result)) {
					valid = false;
					break;
				}
				ttuple = ttuple->next;
				etuple = etuple->next;
			}
			if (!ttuple && valid) {
				expr->result = type_dealias(ctx, tu->type);
				break;
			}
		}
		if (!expr->result) {
			error(ctx, aexpr->loc, expr,
				"Tuple value is not assignable to tagged union hint");
			return;
		}
	} else {
		expr->result = type_store_lookup_tuple(ctx->store,
				aexpr->loc, &result);
		if (expr->result == &builtin_type_error) {
			// an error occurred
			return;
		}
	}

	ttuple = &type_dealias(ctx, expr->result)->tuple;
	struct expression_tuple *etuple = &expr->tuple;
	const struct ast_expression_tuple *atuple = &aexpr->tuple;
	while (etuple) {
		if (!ttuple) {
			error(ctx, atuple->expr->loc, expr,
				"Too many values for tuple type");
			return;
		}
		if (!type_is_assignable(ctx, ttuple->type, etuple->value->result)) {
			error(ctx, atuple->expr->loc, expr,
				"Value is not assignable to tuple value type");
			return;
		}
		etuple->value = lower_implicit_cast(ctx, ttuple->type, etuple->value);
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
	if (operand->result->storage == STORAGE_ERROR) {
		mkerror(expr->unarithm.operand->loc, expr);
		return;
	}

	switch (expr->unarithm.op) {
	case UN_LNOT:
		if (type_dealias(ctx, operand->result)->storage != STORAGE_BOOL) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot perform logical NOT (!) on non-boolean type");
			return;
		}
		expr->result = &builtin_type_bool;
		break;
	case UN_BNOT:
		if (!type_is_integer(ctx, operand->result)) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot perform binary NOT (~) on non-integer type");
			return;
		}
		expr->result = operand->result;
		break;
	case UN_MINUS:
		if (!type_is_numeric(ctx, operand->result)) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot perform operation on non-numeric type");
			return;
		}
		if (operand->result->storage == STORAGE_ICONST) {
			// Not technically quite right, but we need
			// operand->result to be lowered with expr->result, and
			// this is correct enough
			const struct type *old = operand->result;
			const struct type *new = type_create_const(
				STORAGE_ICONST, -old->_const.min,
				-old->_const.max);
			lower_const(ctx, old, new);
		}
		expr->result = operand->result;
		break;
	case UN_ADDRESS:;
		const struct type *result = type_dealias(ctx, operand->result);
		if (result->storage == STORAGE_VOID) {
			error(ctx, aexpr->loc, expr,
				"Can't take address of void");
			return;
		}
		const struct type *ptrhint = NULL;
		if (hint && type_dealias(ctx, hint)->storage == STORAGE_POINTER) {
			ptrhint = type_dealias(ctx, hint)->pointer.referent;
			if (ptrhint->storage == STORAGE_VOID) {
				ptrhint = NULL;
			}
		}
		if (type_is_constant(operand->result)) {
			operand->result = lower_const(ctx, operand->result, ptrhint);
		} else if (ptrhint) {
			// XXX: this is dumb, but we're gonna get rid of the
			// const flag anyway so it doesn't matter
			struct type stripped_result = *result;
			stripped_result.flags &= ~TYPE_CONST;
			stripped_result.id = type_hash(&stripped_result);
			struct type stripped_ptrhint = *type_dealias(ctx, ptrhint);
			stripped_ptrhint.flags &= ~TYPE_CONST;
			stripped_ptrhint.id = type_hash(&stripped_ptrhint);

			if (stripped_result.id == stripped_ptrhint.id) {
				operand->result = ptrhint;
			}
		}
		expr->result = type_store_lookup_pointer(
			ctx->store, aexpr->loc, operand->result, 0);
		break;
	case UN_DEREF:
		if (type_dealias(ctx, operand->result)->storage != STORAGE_POINTER) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot de-reference non-pointer type");
			return;
		}
		if (type_dealias(ctx, operand->result)->pointer.flags
				& PTR_NULLABLE) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot dereference nullable pointer type");
			return;
		}
		if (type_dealias(ctx, operand->result)->pointer.referent->size == 0) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot dereference pointer to zero-sized type");
			return;
		}
		if (type_dealias(ctx, operand->result)->pointer.referent->size
				== SIZE_UNDEFINED) {
			error(ctx, aexpr->unarithm.operand->loc, expr,
				"Cannot dereference pointer to type of undefined size");
			return;
		}
		expr->result = type_dealias(ctx, operand->result)->pointer.referent;
		break;
	}
}

static void
check_expr_vastart(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	if (ctx->fntype->func.variadism != VARIADISM_C) {
		error(ctx, aexpr->loc, expr,
			"Cannot use vastart within function which does not use C-style variadism");
		return;
	}
	expr->type = EXPR_VASTART;
	expr->result = &builtin_type_valist;
}

static void
check_expr_vaarg(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_VAARG;
	if (hint == NULL) {
		error(ctx, aexpr->loc, expr,
			"Cannot infer type of vaarg without hint");
		return;
	}
	expr->vaarg.ap = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->vaarg.ap, expr->vaarg.ap, &builtin_type_valist);
	if (type_dealias(ctx, expr->vaarg.ap->result)->storage != STORAGE_VALIST) {
		error(ctx, aexpr->loc, expr,
			"Expected vaarg operand to be valist");
		return;
	}
	expr->result = hint;
}

static void
check_expr_vaend(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr,
	const struct type *hint)
{
	expr->type = EXPR_VAEND;
	expr->vaarg.ap = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->vaarg.ap, expr->vaarg.ap, &builtin_type_valist);
	if (type_dealias(ctx, expr->vaarg.ap->result)->storage != STORAGE_VALIST) {
		error(ctx, aexpr->loc, expr,
			"Expected vaend operand to be valist");
		return;
	}
	expr->result = &builtin_type_void;
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
		check_expr_append_insert(ctx, aexpr, expr, hint);
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
		check_expr_append_insert(ctx, aexpr, expr, hint);
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
	case EXPR_VAARG:
		check_expr_vaarg(ctx, aexpr, expr, hint);
		break;
	case EXPR_VAEND:
		check_expr_vaend(ctx, aexpr, expr, hint);
		break;
	case EXPR_VASTART:
		check_expr_vastart(ctx, aexpr, expr, hint);
		break;
	}
	assert(expr->result);
	const_refer(expr->result, &expr->result);
}

static void
append_decl(struct context *ctx, struct declaration *decl)
{
	struct declarations *decls = xcalloc(1, sizeof(struct declarations));
	decls->decl = *decl;
	decls->next = ctx->decls;
	ctx->decls = decls;
}

void
check_function(struct context *ctx,
	const struct scope_object *obj,
	const struct ast_decl *adecl)
{
	const struct ast_function_decl *afndecl = &adecl->function;
	ctx->fntype = obj->type;
	if (ctx->fntype->storage == STORAGE_ERROR) {
		return;
	}

	struct declaration _decl, *decl = &_decl;
	decl->decl_type = DECL_FUNC;
	decl->func.type = obj->type;
	decl->func.flags = afndecl->flags;
	decl->exported = adecl->exported;
	decl->loc = adecl->loc;

	decl->symbol = ident_to_sym(&obj->ident);
	mkident(ctx, &decl->ident, &afndecl->ident, NULL);

	if (!adecl->function.body) {
		if (decl->func.flags != 0) {
			error(ctx, adecl->loc, NULL,
				"Function attributes cannot be used on prototypes");
			return;
		}
		decl->func.body = NULL;
		goto end; // Prototype
	}
	if (afndecl->symbol != NULL && decl->func.flags != 0) {
		error(ctx, adecl->loc, NULL,
			"@symbol cannot be used alongside other function attributes");
	}

	decl->func.scope = scope_push(&ctx->scope, SCOPE_FUNC);
	struct ast_function_parameters *params = afndecl->prototype.params;
	while (params) {
		if (!params->name) {
			error(ctx, params->loc, NULL,
				"Function parameters must be named");
			return;
		}
		struct identifier ident = {
			.name = params->name,
		};
		const struct type *type = type_store_lookup_atype(
				ctx->store, params->type);
		if (obj->type->func.variadism == VARIADISM_HARE
				&& !params->next) {
			type = type_store_lookup_slice(ctx->store,
				params->loc, type);
		}
		scope_insert(decl->func.scope, O_BIND,
			&ident, &ident, type, NULL);
		params = params->next;
	}

	struct expression *body = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, afndecl->body, body, obj->type->func.result);
	// TODO: Pass errors up and deal with them at the end of check
	handle_errors(ctx->errors);

	if (!body->terminates && !type_is_assignable(ctx, obj->type->func.result, body->result)) {
		char *restypename = gen_typename(body->result);
		char *fntypename = gen_typename(obj->type->func.result);
		error(ctx, afndecl->body->loc, body,
			"Result value %s is not assignable to function result type %s",
			restypename, fntypename);
		free(restypename);
		free(fntypename);
		return;
	}
	decl->func.body = lower_implicit_cast(ctx, obj->type->func.result, body);

	// TODO: Add function name to errors
	if (decl->func.flags != 0) {
		const char *flag = NULL;
		switch (decl->func.flags) {
		case FN_INIT:
			flag = "@init";
			break;
		case FN_FINI:
			flag = "@fini";
			break;
		case FN_TEST:
			flag = "@test";
			break;
		default:
			error(ctx, adecl->loc, NULL,
				"Only one of @init, @fini, or @test may be used in a function declaration");
			return;
		};
		if (obj->type->func.result != &builtin_type_void) {
			error(ctx, adecl->loc, NULL, "%s function must return void", flag);
		}
		if (obj->type->func.flags & FN_NORETURN) {
			error(ctx, adecl->loc, NULL, "%s function must return", flag);
		}
		if (decl->exported) {
			error(ctx, adecl->loc, NULL, "%s function cannot be exported", flag);
		}
		if (afndecl->prototype.params) {
			error(ctx, adecl->loc, NULL, "%s function cannot have parameters", flag);
		}
	}
	if (obj->type->func.flags & FN_NORETURN && obj->type->func.result != &builtin_type_void) {
		error(ctx, adecl->loc, NULL, "@noreturn function must return void");
	};

	scope_pop(&ctx->scope);
	ctx->fntype = NULL;
end:
	if (((adecl->function.flags & FN_TEST) && !ctx->is_test) || ctx->errors) {
		return;
	}
	append_decl(ctx, decl);
}

static struct incomplete_declaration *
incomplete_declaration_create(struct context *ctx, struct location loc,
		struct scope *scope, const struct identifier *ident,
		const struct identifier *name)
{
	struct scope *subunit = ctx->unit->parent;
	ctx->unit->parent = NULL;
	struct incomplete_declaration *idecl =
		(struct incomplete_declaration *)scope_lookup(scope, name);
	ctx->unit->parent = subunit;

	if (idecl) {
		error_norec(ctx, loc, "Duplicate global identifier '%s'",
			identifier_unparse(ident));
	}
	idecl =  xcalloc(1, sizeof(struct incomplete_declaration));

	scope_object_init((struct scope_object *)idecl, O_SCAN,
			ident, name, NULL, NULL);
	scope_insert_from_object(scope, (struct scope_object *)idecl);
	return idecl;
}

static void
scan_enum_field(struct context *ctx, struct scope *imports,
		struct scope *enum_scope, const struct type *etype,
		struct ast_enum_field *f)
{
	// We have to process the last field first
	// This way, objects in enum_scope will have lnext pointing to
	// the previous element, which is important for implicit enum values.
	if (f->next) {
		scan_enum_field(ctx, imports, enum_scope,
			etype, f->next);
	}
	assert(etype->storage == STORAGE_ENUM);
	struct incomplete_enum_field *field =
		xcalloc(1, sizeof(struct incomplete_enum_field));
	*field = (struct incomplete_enum_field){
		.field = f,
		.enum_scope = enum_scope,
	};

	struct identifier localname = {
		.name = (char *)f->name,
	};
	struct identifier name = {
		.name = (char *)f->name,
		.ns = (struct identifier *)&etype->alias.name,
	};
	struct incomplete_declaration *fld =
		incomplete_declaration_create(ctx, f->loc, enum_scope,
				&name, &localname);
	fld->type = IDECL_ENUM_FLD;
	fld->imports = imports;
	fld->obj.type = etype,
	fld->field = field;
}

static void
scan_types(struct context *ctx, struct scope *imp, struct ast_decl *decl)
{
	for (struct ast_type_decl *t = &decl->type; t; t = t->next) {
		struct identifier with_ns = {0};
		mkident(ctx, &with_ns, &t->ident, NULL);
		struct incomplete_declaration *idecl =
			incomplete_declaration_create(ctx, decl->loc, ctx->scope,
					&with_ns, &t->ident);
		idecl->decl = (struct ast_decl){
			.decl_type = ADECL_TYPE,
			.loc = decl->loc,
			.type = *t,
			.exported = decl->exported,
		};
		idecl->imports = imp;
		if (t->type->storage == STORAGE_ENUM) {
			bool exported = idecl->decl.exported;
			const struct type *type = type_store_lookup_enum(
					ctx->store, t->type, exported);
			if (type->storage == STORAGE_VOID) {
				return; // error occured
			}
			scope_push((struct scope **)&type->_enum.values, SCOPE_ENUM);
			scan_enum_field(ctx, imp,
				type->_enum.values, type, t->type->_enum.values);
			type->_enum.values->parent = ctx->defines;
			idecl->obj.otype = O_TYPE;
			idecl->obj.type = type;
			append_decl(ctx, &(struct declaration){
				.decl_type = DECL_TYPE,
				.ident = idecl->obj.ident,
				.loc = decl->loc,
				.exported = exported,
				.type = type,
			});
		} else {
			idecl->type = IDECL_DECL;
		}
	}
}

void
resolve_const(struct context *ctx, struct incomplete_declaration *idecl)
{
	const struct ast_global_decl *decl = &idecl->decl.global;

	assert(!decl->symbol); // Invariant

	const struct type *type = NULL;
	if (decl->type) {
		type = type_store_lookup_atype(ctx->store, decl->type);
	}
	struct expression *init = xcalloc(1, sizeof(struct expression)),
		*value = xcalloc(1, sizeof(struct expression));
	check_expression(ctx, decl->init, init, type);
	if (!decl->type) {
		// XXX: Do we need to do anything more here
		type = lower_const(ctx, init->result, NULL);
		if (type->storage == STORAGE_NULL) {
			error(ctx, decl->init->loc, value,
				"Null is not a valid type for a constant");
			type = &builtin_type_error;
			goto end;
		}
	}
	if (!type_is_assignable(ctx, type, init->result)) {
		char *typename1 = gen_typename(init->result);
		char *typename2 = gen_typename(type);
		error(ctx, decl->init->loc, value,
			"Initializer type %s is not assignable to constant type %s",
			typename1, typename2);
		free(typename1);
		free(typename2);
		type = &builtin_type_error;
		goto end;
	}
	if (decl->type && decl->type->storage == STORAGE_ARRAY
			&& decl->type->array.contextual) {
		type = lower_const(ctx, init->result, NULL);
	} else {
		init = lower_implicit_cast(ctx, type, init);
	}
	assert(type->size != SIZE_UNDEFINED);

	enum eval_result r = eval_expr(ctx, init, value);
	if (r != EVAL_OK) {
		error(ctx, decl->init->loc, value,
			"Unable to evaluate constant init at compile time");
		type = &builtin_type_error;
		goto end;
	}
end:
	idecl->obj.otype = O_CONST;
	idecl->obj.type = type;
	idecl->obj.value = value;

	if (!ctx->defines || ctx->errors) {
		return;
	}
	const struct scope_object *shadow_obj =
		scope_lookup(ctx->defines, &idecl->obj.ident);
	if (shadow_obj && &idecl->obj != shadow_obj) {
		// Shadowed by define
		if (idecl->obj.type != shadow_obj->type) {
			char *typename = gen_typename(idecl->obj.type);
			char *shadow_typename = gen_typename(shadow_obj->type);
			error(ctx, idecl->decl.loc, NULL,
					"Constant of type %s is shadowed by define of incompatible type %s",
					typename, shadow_typename);
			free(typename);
			free(shadow_typename);
		}
		idecl->obj.value = shadow_obj->value;
	}
	append_decl(ctx, &(struct declaration){
		.decl_type = DECL_CONST,
		.ident = idecl->obj.ident,
		.exported = idecl->decl.exported,
		.loc = idecl->decl.loc,
		.constant = {
			.type = type,
			.value = value,
		}
	});
}

void
resolve_function(struct context *ctx, struct incomplete_declaration *idecl)
{
	const struct ast_function_decl *decl = &idecl->decl.function;

	const struct ast_type fn_atype = {
		.storage = STORAGE_FUNCTION,
		.flags = 0,
		.func = decl->prototype,
	};
	const struct type *fntype = type_store_lookup_atype(
			ctx->store, &fn_atype);

	idecl->obj.otype = O_DECL;
	idecl->obj.type = fntype;
}

void
resolve_global(struct context *ctx, struct incomplete_declaration *idecl)
{
	const struct ast_global_decl *decl = &idecl->decl.global;
	const struct type *type = NULL;
	bool context = false;
	struct expression *init, *value = NULL;
	if (decl->type) {
		type = type_store_lookup_atype(ctx->store, decl->type);
		context = decl->type->storage == STORAGE_ARRAY
			&& decl->type->array.contextual;
		if (context && !decl->init) {
			error(ctx, decl->type->loc, NULL,
				"Cannot infer array length without an init");
			type = &builtin_type_error;
			goto end;
		}
	}

	if (decl->init) {
		init = xcalloc(1, sizeof(struct expression));
		value = xcalloc(1, sizeof(struct expression));
		check_expression(ctx, decl->init, init, type);
		if (type) {
			if (!type_is_assignable(ctx, type, init->result)) {
				char *typename1 = gen_typename(init->result);
				char *typename2 = gen_typename(type);
				error(ctx, decl->init->loc, value,
					"Initializer type %s is not assignable to constant type %s",
					typename1, typename2);
				free(typename1);
				free(typename2);
				type = &builtin_type_error;
				goto end;
			}
		} else {
			type = lower_const(ctx, init->result, NULL);
		}
		if (context) {
			type = init->result;
		} else {
			init = lower_implicit_cast(ctx, type, init);
		}
		assert(type->size != SIZE_UNDEFINED);
		if (type->storage == STORAGE_NULL) {
			error(ctx, decl->init->loc, NULL,
				"Null is not a valid type for a global");
			type = &builtin_type_error;
			goto end;
		}
		enum eval_result r = eval_expr(ctx, init, value);
		if (r != EVAL_OK) {
			error(ctx, decl->init->loc, value,
				"Unable to evaluate constant init at compile time");
			type = &builtin_type_error;
			goto end;
		}
	}

end:
	idecl->obj.otype = O_DECL;
	idecl->obj.type = type;
	idecl->obj.threadlocal = decl->threadlocal;

	append_decl(ctx, &(struct declaration){
		.decl_type = DECL_GLOBAL,
		.ident = idecl->obj.ident,
		.symbol = ident_to_sym(&idecl->obj.ident),

		.exported = idecl->decl.exported,
		.loc = idecl->decl.loc,
		.global = {
			.type = type,
			.value = value,
			.threadlocal = idecl->decl.global.threadlocal,
		}
	});
}

static void
resolve_enum_field(struct context *ctx, struct incomplete_declaration *idecl)
{
	assert(idecl->type == IDECL_ENUM_FLD);

	const struct type *type = idecl->obj.type;

	struct identifier localname = {
		.name = idecl->obj.ident.name
	};

	const struct scope_object *new =
		scope_lookup(idecl->field->enum_scope, &localname);
	if (new != &idecl->obj) {
		wrap_resolver(ctx, new, resolve_enum_field);
		assert(new->otype == O_CONST);
		idecl->obj.otype = O_CONST;
		idecl->obj.type = type;
		idecl->obj.value = new->value;
		return;
	}

	ctx->scope = idecl->field->enum_scope;
	struct expression *value = xcalloc(1, sizeof(struct expression));
	value->result = type;
	if (idecl->field->field->value) { // explicit value
		struct expression *initializer =
			xcalloc(1, sizeof(struct expression));
		check_expression(ctx, idecl->field->field->value,
				initializer, type->alias.type);

		if (!type_is_assignable(ctx, type->alias.type, initializer->result)) {
			char *inittypename = gen_typename(initializer->result);
			char *builtintypename = gen_typename(type->alias.type);
			error_norec(ctx, idecl->field->field->value->loc,
				"Enum value type (%s) is not assignable from initializer type (%s) for value %s",
				builtintypename, inittypename, idecl->obj.ident.name);
		}

		initializer = lower_implicit_cast(ctx, type, initializer);
		enum eval_result r = eval_expr(ctx, initializer, value);
		if (r != EVAL_OK) {
			error_norec(ctx, idecl->field->field->value->loc,
				"Unable to evaluate constant initializer at compile time");
		}
	} else { // implicit value
		const struct scope_object *obj = idecl->obj.lnext;
		// find previous enum value
		wrap_resolver(ctx, obj, resolve_enum_field);
		value->type = EXPR_CONSTANT;
		if (type_is_signed(ctx, type_dealias(ctx, type))) {
			if (obj == NULL) {
				value->constant.ival = 0;
			} else {
				value->constant.ival = obj->value->constant.ival + 1;
			}
		} else {
			if (obj == NULL) {
				value->constant.uval = 0;
			} else {
				value->constant.uval = obj->value->constant.uval + 1;
			}
		}
	}

	idecl->obj.otype = O_CONST;
	idecl->obj.value = value;
}

const struct type *
lookup_enum_type(struct context *ctx, const struct scope_object *obj)
{
	const struct type *enum_type = NULL;

	switch (obj->otype) {
	case O_SCAN: {
		struct incomplete_declaration *idecl =
			(struct incomplete_declaration *)obj;

		if (idecl->in_progress) {
			// Type alias cycle will be handled in check
			return NULL;
		}

		if (idecl->type != IDECL_DECL ||
				idecl->decl.decl_type != ADECL_TYPE) {
			return NULL;
		}

		if (idecl->decl.type.type->storage == STORAGE_ENUM) {
			assert(false);
		} else if (idecl->decl.type.type->storage == STORAGE_ALIAS) {
			ctx->scope->parent = idecl->imports;
			const struct identifier *alias =
				&idecl->decl.type.type->alias;
			const struct scope_object *new = scope_lookup(ctx->scope,
					alias);
			if (new) {
				idecl->in_progress = true;
				enum_type = lookup_enum_type(ctx, new);
				idecl->in_progress = false;
			}
		}
		break;
	}
	case O_TYPE:
		enum_type = obj->type;
		break;
	default:
		return NULL;
	}

	if (!enum_type) {
		return NULL;
	}

	enum_type = type_dealias(ctx, enum_type);
	if (enum_type->storage != STORAGE_ENUM) {
		return NULL;
	}
	return enum_type;
}

static void
scan_enum_field_aliases(struct context *ctx, const struct scope_object *obj)
{
	const struct type *enum_type = lookup_enum_type(ctx, obj);

	if (!enum_type) {
		return;
	}

	// orig->type is (perhaps transitively) an alias of a resolved enum
	// type, which means its dependency graph is a linear chain of
	// resolved types ending with that enum, so we can immediately resolve it
	wrap_resolver(ctx, obj, resolve_type);

	for (const struct scope_object *val = enum_type->_enum.values->objects;
			val; val = val->lnext) {
		struct identifier name = {
			.name = val->name.name,
			.ns = (struct identifier *)&obj->name,
		};
		struct identifier ident = {
			.name = val->name.name,
			.ns = (struct identifier *)&obj->ident,
		};
		struct ast_enum_field *afield =
			xcalloc(1, sizeof(struct ast_enum_field));
		*afield = (struct ast_enum_field){
			.loc = (struct location){0}, // XXX: what to put here?
			.name = xstrdup(val->name.name),
		};

		struct incomplete_enum_field *field =
			xcalloc(1, sizeof(struct incomplete_enum_field));
		struct incomplete_declaration *idecl =
			(struct incomplete_declaration *)val;
		*field = (struct incomplete_enum_field){
			.field = afield,
			.enum_scope = idecl->field->enum_scope,
		};

		idecl = incomplete_declaration_create(ctx, (struct location){0},
			ctx->scope, &ident, &name);
		idecl->type = IDECL_ENUM_FLD;
		idecl->obj.type = obj->type;
		idecl->field = field;
	};
}

void
resolve_dimensions(struct context *ctx, struct incomplete_declaration *idecl)
{
	if (idecl->type != IDECL_DECL || idecl->decl.decl_type != ADECL_TYPE) {
		struct location loc;
		if (idecl->type == IDECL_ENUM_FLD) {
			loc = idecl->field->field->loc;
		} else {
			loc = idecl->decl.loc;
		}
		error_norec(ctx, loc, "'%s' is not a type",
				identifier_unparse(&idecl->obj.name));
	}
	struct dimensions dim = type_store_lookup_dimensions(ctx->store,
			idecl->decl.type.type);
	idecl->obj.type = xcalloc(1, sizeof(struct type));
	*(struct type *)idecl->obj.type = (struct type){
		.size = dim.size,
		.align = dim.align,
	};
}

void
resolve_type(struct context *ctx, struct incomplete_declaration *idecl)
{
	if (idecl->type != IDECL_DECL || idecl->decl.decl_type != ADECL_TYPE) {
		struct location loc;
		if (idecl->type == IDECL_ENUM_FLD) {
			loc = idecl->field->field->loc;
		} else {
			loc = idecl->decl.loc;
		}
		error_norec(ctx, loc, "'%s' is not a type",
				identifier_unparse(&idecl->obj.name));
	}

	// 1. compute type dimensions
	struct dimensions dim = type_store_lookup_dimensions(
			ctx->store, idecl->decl.type.type);

	handle_errors(ctx->errors);
	idecl->in_progress = false;

	// 2. compute type representation and store it
	struct type _alias = {
		.storage = STORAGE_ALIAS,
		.alias = {
			.ident = idecl->obj.ident,
			.name = idecl->obj.name,
			.type = NULL,
			.exported = idecl->decl.exported,
		},
		.size = dim.size,
		.align = dim.align,
		.flags = idecl->decl.type.type->flags,
	};

	const struct type *alias = type_store_lookup_alias(
			ctx->store, &_alias, &dim);
	idecl->obj.otype = O_TYPE;
	idecl->obj.type = alias;
	((struct type *)alias)->alias.type =
		type_store_lookup_atype(ctx->store, idecl->decl.type.type);
	assert(alias->alias.type != NULL);

	append_decl(ctx, &(struct declaration){
		.decl_type = DECL_TYPE,
		.ident = idecl->obj.ident,
		.loc = idecl->decl.loc,
		.exported = idecl->decl.exported,
		.type = alias,
	});
}

static struct incomplete_declaration *
scan_const(struct context *ctx, struct scope *imports, bool exported,
		struct location loc, struct ast_global_decl *decl)
{
	struct identifier with_ns = {0};
	mkident(ctx, &with_ns, &decl->ident, NULL);
	struct incomplete_declaration *idecl =
		incomplete_declaration_create(ctx, loc,
				ctx->scope, &with_ns, &decl->ident);
	idecl->type = IDECL_DECL;
	idecl->decl = (struct ast_decl){
		.decl_type = ADECL_CONST,
		.loc = loc,
		.constant = *decl,
		.exported = exported,
	};
	idecl->imports = imports;
	return idecl;
}

static void
scan_decl(struct context *ctx, struct scope *imports, struct ast_decl *decl)
{
	struct incomplete_declaration *idecl = {0};
	struct identifier ident = {0};
	switch (decl->decl_type) {
	case ADECL_CONST:
		for (struct ast_global_decl *g = &decl->constant; g; g = g->next) {
			scan_const(ctx, imports, decl->exported, decl->loc, g);
		}
		break;
	case ADECL_GLOBAL:
		for (struct ast_global_decl *g = &decl->global; g; g = g->next) {
			mkident(ctx, &ident, &g->ident, g->symbol);
			idecl = incomplete_declaration_create(ctx, decl->loc,
				ctx->scope, &ident, &g->ident);
			idecl->type = IDECL_DECL;
			idecl->decl = (struct ast_decl){
				.decl_type = ADECL_GLOBAL,
				.loc = decl->loc,
				.global = *g,
				.exported = decl->exported,
			};
			idecl->imports = imports;
		}
		break;
	case ADECL_FUNC:;
		struct ast_function_decl *func = &decl->function;
		struct identifier *name = NULL;
		if (func->flags) {
			const char *template = NULL;
			if (func->flags & FN_TEST) {
				template = "testfunc.%d";
			} else if (func->flags & FN_INIT) {
				template = "initfunc.%d";
			} else if (func->flags & FN_FINI) {
				template = "finifunc.%d";
			}
			assert(template);
			ident.name = gen_name(&ctx->id, template);
			++ctx->id;

			name = &ident;
		} else {
			mkident(ctx, &ident, &func->ident, func->symbol);
			name = &func->ident;
		}
		idecl = incomplete_declaration_create(ctx, decl->loc,
			ctx->scope, &ident, name);
		idecl->type = IDECL_DECL;
		idecl->decl = (struct ast_decl){
			.decl_type = ADECL_FUNC,
			.loc = decl->loc,
			.function = *func,
			.exported = decl->exported,
		};
		idecl->imports = imports;
		break;
	case ADECL_TYPE:
		scan_types(ctx, imports, decl);
		break;
	case ADECL_ASSERT:;
		static uint64_t num = 0;
		int n = snprintf(NULL, 0, "static assert %" SCNu64, num);
		ident.name = xcalloc(n + 1, sizeof(char));
		snprintf(ident.name, n + 1, "static assert %" SCNu64, num);
		++num;
		idecl = incomplete_declaration_create(ctx, decl->loc,
			ctx->scope, &ident, &ident);
		idecl->type = IDECL_DECL;
		idecl->decl = (struct ast_decl){
			.decl_type = ADECL_ASSERT,
			.loc = decl->loc,
			.assert = decl->assert,
			.exported = decl->exported,
		};
		idecl->imports = imports;
		break;
	}
}

void
resolve_decl(struct context *ctx, struct incomplete_declaration *idecl)
{
	switch (idecl->type) {
	case IDECL_ENUM_FLD:
		resolve_enum_field(ctx, idecl);
		return;
	case IDECL_DECL:
		break;
	}

	switch (idecl->decl.decl_type) {
	case ADECL_CONST:
		resolve_const(ctx, idecl);
		return;
	case ADECL_GLOBAL:
		resolve_global(ctx, idecl);
		return;
	case ADECL_FUNC:
		resolve_function(ctx, idecl);
		return;
	case ADECL_TYPE:
		resolve_type(ctx, idecl);
		return;
	case ADECL_ASSERT:;
		struct expression expr = {0};
		check_assert(ctx, idecl->decl.assert, idecl->decl.loc, &expr);
		return;
	}
	abort();
}

void
wrap_resolver(struct context *ctx, const struct scope_object *obj,
	resolvefn resolver)
{
	// save current subunit and enum context
	struct scope *scope = ctx->scope;
	struct scope *subunit = ctx->unit->parent;
	ctx->unit->parent = NULL;
	const struct type *fntype = ctx->fntype;
	ctx->fntype = NULL;
	bool deferring = ctx->deferring;
	ctx->deferring = false;

	// ensure this declaration wasn't already scanned
	if (!obj || obj->otype != O_SCAN) {
		goto exit;
	}

	struct incomplete_declaration *idecl = (struct incomplete_declaration *)obj;

	// load this declaration's subunit context
	ctx->scope = ctx->defines;
	ctx->unit->parent = idecl->imports;

	// resolving a declaration that is already in progress -> cycle
	if (idecl->in_progress) {
		struct location loc;
		if (idecl->type == IDECL_ENUM_FLD) {
			loc = idecl->field->field->loc;
		} else {
			loc = idecl->decl.loc;
		}
		error_norec(ctx, loc, "Circular dependency for '%s'",
			identifier_unparse(&idecl->obj.name));
	}
	idecl->in_progress = true;

	resolver(ctx, idecl);

	idecl->in_progress = false;
exit:
	// load stored context
	ctx->deferring = deferring;
	ctx->fntype = fntype;
	ctx->unit->parent = subunit;
	ctx->scope = scope;
}

static void
load_import(struct context *ctx, struct ast_global_decl *defines,
	struct ast_imports *import, struct type_store *ts, struct scope *scope)
{
	struct context *old_ctx = ctx->store->check_context;
	struct scope *mod = module_resolve(ctx->modcache,
			defines, &import->ident, ts);
	ctx->store->check_context = old_ctx;

	struct identifier _ident = {0};
	struct identifier *mod_ident = &_ident;
	if (import->mode & (AST_IMPORT_WILDCARD | AST_IMPORT_ALIAS)) {
		mod_ident = import->alias;
	} else {
		mod_ident->name = import->ident.name;
	}
	if (import->mode & AST_IMPORT_MEMBERS) {
		assert(!(import->mode & AST_IMPORT_WILDCARD));
		for (struct ast_imports *member = import->members;
				member; member = member->next) {
			struct identifier name = {
				.name = member->alias?
					member->alias->name : member->ident.name,
				.ns = import->alias,
			};
			struct identifier ident = {
				.name = member->ident.name,
				.ns = &import->ident,
			};
			const struct scope_object *obj = scope_lookup(mod, &ident);
			if (!obj) {
				error_norec(ctx, member->loc, "Unknown object '%s'",
						identifier_unparse(&ident));
			}
			struct scope_object *new = scope_insert(
					scope, obj->otype, &obj->ident,
					&name, obj->type, obj->value);
			new->threadlocal = obj->threadlocal;
			if (obj->otype != O_TYPE
					|| type_dealias(ctx, obj->type)->storage
						!= STORAGE_ENUM) {
				continue;
			};
			struct scope *enum_scope =
				type_dealias(ctx, obj->type)->_enum.values;
			for (struct scope_object *o = enum_scope->objects;
					o; o = o->lnext) {
				struct identifier value_ident = {
					.name = o->name.name,
					.ns = &ident,
				};
				struct identifier value_name = {
					.name = o->name.name,
					.ns = &name,
				};
				scope_insert(scope, o->otype, &value_ident,
					&value_name, o->type, o->value);
			};

		}
	} else {
		for (struct scope_object *obj = mod->objects;
				obj; obj = obj->lnext) {
			assert(obj->otype != O_SCAN);

			struct scope_object *new;
			if (!(import->mode & AST_IMPORT_ALIAS)
					&& import->ident.ns != NULL) {
				new = scope_insert(scope, obj->otype, &obj->ident,
					&obj->name, obj->type, obj->value);
				new->threadlocal = obj->threadlocal;
			}

			struct identifier ns, name = {
				.name = obj->name.name,
				.ns = mod_ident,
			};
			if (type_dealias(ctx, obj->type)->storage == STORAGE_ENUM
					&& obj->otype == O_CONST) {
				ns = (struct identifier){
					.name = obj->name.ns->name,
					.ns = mod_ident,
				};
				name.ns = &ns;
			};
			new = scope_insert(scope, obj->otype, &obj->ident,
				&name, obj->type, obj->value);
			new->threadlocal = obj->threadlocal;
		}
	}
}

static const struct location defineloc = {
	.file = 0,
	.lineno = 1,
	.colno = 1,
};

struct scope *
check_internal(struct type_store *ts,
	struct modcache **cache,
	bool is_test,
	struct ast_global_decl *defines,
	const struct ast_unit *aunit,
	struct unit *unit,
	bool scan_only)
{
	struct context ctx = {0};
	ctx.ns = unit->ns;
	ctx.is_test = is_test;
	ctx.store = ts;
	ctx.store->check_context = &ctx;
	ctx.next = &ctx.errors;
	ctx.modcache = cache;

	// Top-level scope management involves:
	//
	// - Creating a top-level scope for the whole unit, to which
	//   declarations are added.
	// - Creating a scope for each sub-unit, and populating it with imports.
	//
	// Further down the call frame, subsequent functions will create
	// sub-scopes for each declaration, expression-list, etc.

	// Put defines into a temporary scope (-D on the command line)
	sources[0] = "-D";
	ctx.scope = NULL;
	ctx.unit = scope_push(&ctx.scope, SCOPE_DEFINES);
	for (struct ast_global_decl *def = defines; def; def = def->next) {
		struct incomplete_declaration *idecl =
			scan_const(&ctx, NULL, false , defineloc, def);
		resolve_const(&ctx, idecl);
	}
	ctx.defines = ctx.scope;
	ctx.scope = NULL;
	ctx.defines->parent = ctx.unit = scope_push(&ctx.scope, SCOPE_UNIT);
	sources[0] = "<unknown>";

	// Populate the imports and put declarations into a scope.
	// Each declaration holds a reference to its subunit's imports
	// A scope gets us:
	//  a) duplicate detection for free
	//  b) a way to find declaration's definition when it's refered to
	struct scopes *subunit_scopes = NULL, **next = &subunit_scopes;
	struct scope *su_scope = NULL;
	struct identifiers **inext = &unit->imports;
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		su_scope = NULL;
		scope_push(&su_scope, SCOPE_SUBUNIT);
		for (struct ast_imports *imports = su->imports;
				imports; imports = imports->next) {
			load_import(&ctx, defines, imports, ts, su_scope);

			bool found = false;
			for (struct identifiers *uimports = unit->imports;
					uimports; uimports = uimports->next) {
				if (identifier_eq(&uimports->ident, &imports->ident)) {
					found = true;
					break;
				}
			}
			if (!found) {
				struct identifiers *uimport = *inext =
					xcalloc(1, sizeof(struct identifiers));
				identifier_dup(&uimport->ident, &imports->ident);
				inext = &uimport->next;
			}
		}

		for (struct ast_decls *d = su->decls; d; d = d->next) {
			scan_decl(&ctx, su_scope, &d->decl);
		};

		*next = xcalloc(1, sizeof(struct scopes));
		(*next)->scope = su_scope;
		next = &(*next)->next;
	}

	// Find enum aliases and store them in incomplete enum value declarations
	for (const struct scope_object *obj = ctx.scope->objects;
			obj; obj = obj->lnext) {
		scan_enum_field_aliases(&ctx, obj);
	}

	// XXX: shadowed declarations are not checked for consistency
	ctx.scope = ctx.defines;

	for (const struct scope_object *obj = ctx.scope->objects;
			obj; obj = obj->lnext) {
		const struct scope_object *shadowed_obj =
			scope_lookup(ctx.unit, &obj->name);
		if (!shadowed_obj) {
			continue;
		}
		if (shadowed_obj->otype == O_CONST) {
			continue;
		}
		if (shadowed_obj->otype == O_SCAN) {
			const struct incomplete_declaration *idecl =
				(struct incomplete_declaration *)shadowed_obj;
			if (idecl->type == IDECL_DECL &&
					idecl->decl.decl_type == ADECL_CONST) {
				continue;
			}
		}
		error(&ctx, defineloc, NULL, "Define shadows a non-define object");
	}

	// Perform actual declaration resolution
	for (const struct scope_object *obj = ctx.unit->objects;
			obj; obj = obj->lnext) {
		wrap_resolver(&ctx, obj, resolve_decl);
		// populate the expression graph
		struct incomplete_declaration *idecl =
			(struct incomplete_declaration *)obj;
		if (idecl->type == IDECL_DECL && idecl->decl.decl_type == ADECL_FUNC) {
			ctx.unit->parent = idecl->imports;
			check_function(&ctx, &idecl->obj, &idecl->decl);
		}
	}

	handle_errors(ctx.errors);
	unit->declarations = ctx.decls;

	if (!(scan_only || unit->declarations)) {
		xfprintf(stderr, "Error: module contains no declarations\n");
		exit(EXIT_FAILURE);
	}

	ctx.store->check_context = NULL;
	ctx.unit->parent = NULL;
	return ctx.unit;
}

struct scope *
check(struct type_store *ts,
	bool is_test,
	struct ast_global_decl *defines,
	const struct ast_unit *aunit,
	struct unit *unit)
{
	struct modcache *modcache[MODCACHE_BUCKETS];
	memset(modcache, 0, sizeof(modcache));
	return check_internal(ts, modcache, is_test, defines, aunit, unit, false);
}
