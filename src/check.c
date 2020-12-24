#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"
#include "check.h"
#include "expr.h"
#include "scope.h"
#include "trace.h"
#include "type_store.h"
#include "types.h"

struct context {
	struct type_store store;
	struct scope *unit;
	struct scope *scope;
};

static void
expect(bool constraint, char *fmt, ...)
{
	// TODO: Bring along line numbers and such
	if (!constraint) {
		va_list ap;
		va_start(ap, fmt);

		fprintf(stderr, "Error: ");
		vfprintf(stderr, fmt, ap);
		fprintf(stderr, "\n");
		abort();
	}
}

static void check_expression(struct context *ctx,
	const struct ast_expression *aexpr, struct expression *expr);

static void
check_expr_access(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	char buf[1024];
	identifier_unparse_static(&aexpr->access.ident, buf, sizeof(buf));

	trace(TR_CHECK, "access %s", buf);
	expr->type = EXPR_ACCESS;

	const struct scope_object *obj = scope_lookup(
		ctx->scope, &aexpr->access.ident);
	expect(obj, "Unknown object %s", buf);

	expr->result = obj->type;
	expr->access.object = obj;
}

static void
check_expr_assign(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trace(TR_CHECK, "assign");
	expr->type = EXPR_ASSIGN;
	expr->result = &builtin_type_void;
	expr->assign.indirect = aexpr->assign.indirect;
	struct expression *object = calloc(1, sizeof(struct expression));
	struct expression *value = calloc(1, sizeof(struct expression));

	check_expression(ctx, aexpr->assign.object, object);
	check_expression(ctx, aexpr->assign.value, value);

	expr->assign.object = object;
	expr->assign.value = value;

	if (aexpr->assign.indirect) {
		expect(object->result->storage == TYPE_STORAGE_POINTER,
			"Cannot dereference non-pointer type for assignment");
		expect(!(object->result->pointer.flags & PTR_NULLABLE),
			"Cannot dereference nullable pointer type");
		// TODO: Test assignability rules
		assert(object->result->pointer.referent->storage == value->result->storage);
	} else {
		assert(object->type == EXPR_ACCESS); // Invariant
		const struct scope_object *obj = object->access.object;
		expect(!(obj->type->flags & TYPE_CONST), "Cannot assign to const object");
		// TODO: Test assignability rules:
		assert(obj->type->storage == value->result->storage);
	}
}

static void
check_expr_binarithm(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trace(TR_CHECK, "binarithm");
	expr->type = EXPR_BINARITHM;
	expr->binarithm.op = aexpr->binarithm.op;

	struct expression *lvalue = calloc(1, sizeof(struct expression)),
		*rvalue = calloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->binarithm.lvalue, lvalue);
	check_expression(ctx, aexpr->binarithm.rvalue, rvalue);
	expr->binarithm.lvalue = lvalue;
	expr->binarithm.rvalue = rvalue;

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
		// TODO: Promotion
		assert(lvalue->result->storage == rvalue->result->storage);
		expr->result = lvalue->result;
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
		expect(lvalue->result == &builtin_type_bool
				&& rvalue->result == &builtin_type_bool,
			"Logical expressions must have boolean operands");
		assert(0); // TODO
	}
}

static void
check_expr_binding(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trace(TR_CHECK, "binding");
	expr->type = EXPR_BINDING;
	expr->result = &builtin_type_void;

	struct expression_binding *binding = &expr->binding;
	struct expression_binding **next = &expr->binding.next;

	const struct ast_expression_binding *abinding = &aexpr->binding;
	while (abinding) {
		struct identifier ident = {
			.name = abinding->name,
		};
		struct expression *initializer =
			calloc(1, sizeof(struct expression));
		check_expression(ctx, abinding->initializer, initializer);

		const struct type *type;
		if (abinding->type) {
			type = type_store_lookup_atype(
				&ctx->store, abinding->type);
			// TODO: Check assignability of initializer
		} else {
			type = type_store_lookup_with_flags(&ctx->store,
				initializer->result, abinding->flags);
		}

		const struct scope_object *obj = scope_insert(ctx->scope,
				O_BIND, &ident, type);
		binding->object = obj;
		binding->initializer = initializer;

		if (abinding->next) {
			binding = *next =
				calloc(1, sizeof(struct expression_binding));
			next = &binding->next;
		}

		abinding = abinding->next;
	}
}

static void
check_expr_call(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trenter(TR_CHECK, "call");
	expr->type = EXPR_CALL;

	struct expression *lvalue = calloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->call.lvalue, lvalue);
	expr->call.lvalue = lvalue;

	const struct type *fntype = type_dereference(lvalue->result);
	expect(fntype->storage == TYPE_STORAGE_FUNCTION,
		"Cannot call non-function type");
	expr->result = fntype->func.result;
	assert(fntype->func.variadism == VARIADISM_NONE); // TODO

	struct call_argument *arg, **next = &expr->call.args;
	struct ast_call_argument *aarg = aexpr->call.args;
	struct type_func_param *param = fntype->func.params;
	while (param && aarg) {
		trenter(TR_CHECK, "arg");
		assert(!aarg->variadic); // TODO
		arg = *next = calloc(1, sizeof(struct call_argument));
		arg->value = calloc(1, sizeof(struct expression));
		check_expression(ctx, aarg->value, arg->value);

		// TODO: Test for assignability
		expect(arg->value->result->storage == param->type->storage,
				"Invalid type for parameter");

		aarg = aarg->next;
		param = param->next;
		next = &arg->next;
		trleave(TR_CHECK, NULL);
	}

	expect(!aarg, "Too many parameters for function call");
	expect(!param, "Not enough parameters for function call");

	trleave(TR_CHECK, NULL);
}

static void
check_expr_constant(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trace(TR_CHECK, "constant");
	expr->type = EXPR_CONSTANT;
	expr->result = builtin_type_for_storage(aexpr->constant.storage, false);

	switch (aexpr->constant.storage) {
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
		expr->constant.ival = aexpr->constant.ival;
		break;
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_SIZE:
		expr->constant.uval = aexpr->constant.uval;
		break;
	case TYPE_STORAGE_RUNE:
		expr->constant.rune = aexpr->constant.rune;
		break;
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_STRING:
		assert(0); // TODO
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // Invariant
	}
}

static void
check_expr_list(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trenter(TR_CHECK, "expression-list");
	expr->type = EXPR_LIST;

	struct scope *scope = scope_push(&ctx->scope, TR_CHECK);
	expr->list.scope = scope;

	struct expressions *list = &expr->list.exprs;
	struct expressions **next = &list->next;

	const struct ast_expression_list *alist = &aexpr->list;
	while (alist) {
		struct expression *lexpr = calloc(1, sizeof(struct expression));
		check_expression(ctx, alist->expr, lexpr);
		list->expr = lexpr;

		alist = alist->next;
		if (alist) {
			*next = calloc(1, sizeof(struct expressions));
			list = *next;
			next = &list->next;
		} else {
			expr->result = lexpr->result;
		}
	}

	scope_pop(&ctx->scope, TR_CHECK);
	trleave(TR_CHECK, NULL);
}

static void
check_expr_measure(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trenter(TR_CHECK, "measure");
	expr->type = EXPR_MEASURE;
	expr->result = &builtin_type_size;
	expr->measure.op = aexpr->measure.op;

	switch (expr->measure.op) {
	case M_LEN:
		expr->measure.value = calloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->measure.value, expr->measure.value);
		enum type_storage vstor = expr->measure.value->result->storage;
		expect(vstor == TYPE_STORAGE_ARRAY || vstor == TYPE_STORAGE_SLICE,
			"len argument must be of an array or slice type");
		// TODO: Check that array type is not unbounded
		break;
	case M_SIZE:
		expr->measure.type = type_store_lookup_atype(
			&ctx->store, aexpr->measure.type);
		break;
	case M_OFFSET:
		assert(0); // TODO
	}
}

static void
check_expr_return(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trenter(TR_CHECK, "return");
	expr->type = EXPR_RETURN;
	expr->result = &builtin_type_void;
	expr->terminates = true;

	if (aexpr->_return.value) {
		struct expression *rval = calloc(1, sizeof(struct expression));
		check_expression(ctx, aexpr->_return.value, rval);
		expr->_return.value = rval;
		// TODO: Test assignability with function's return type
	}

	trleave(TR_CHECK, NULL);
}

static void
check_expr_unarithm(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trenter(TR_CHECK, "unarithm");
	expr->type = EXPR_UNARITHM;

	struct expression *operand = calloc(1, sizeof(struct expression));
	check_expression(ctx, aexpr->unarithm.operand, operand);
	expr->unarithm.operand = operand;
	expr->unarithm.op = aexpr->unarithm.op;

	switch (expr->unarithm.op) {
	case UN_LNOT:
		expect(operand->result->storage == TYPE_STORAGE_BOOL,
			"Cannot perform logical NOT (!) on non-boolean type");
		expr->result = &builtin_type_bool;
		break;
	case UN_BNOT:
		expect(type_is_integer(operand->result),
			"Cannot perform binary NOT (~) on non-integer type");
		expect(!type_is_signed(operand->result),
			"Cannot perform binary NOT (~) on signed type");
		expr->result = operand->result;
		break;
	case UN_MINUS:
	case UN_PLUS:
		expect(type_is_numeric(operand->result),
			"Cannot perform operation on non-numeric type");
		expect(type_is_signed(operand->result),
			"Cannot perform operation on unsigned type");
		expr->result = operand->result;
		break;
	case UN_ADDRESS:
		expr->result = type_store_lookup_pointer(
			&ctx->store, operand->result, 0);
		break;
	case UN_DEREF:
		expect(operand->result->storage == TYPE_STORAGE_POINTER,
			"Cannot de-reference non-pointer type");
		expect(!(operand->result->pointer.flags & PTR_NULLABLE),
			"Cannot dereference nullable pointer type");
		expr->result = operand->result->pointer.referent;
		break;
	}

	trleave(TR_CHECK, NULL);
}

static void
check_expression(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trenter(TR_CHECK, "expression");

	switch (aexpr->type) {
	case EXPR_ACCESS:
		check_expr_access(ctx, aexpr, expr);
		break;
	case EXPR_ASSERT:
		assert(0); // TODO
	case EXPR_ASSIGN:
		check_expr_assign(ctx, aexpr, expr);
		break;
	case EXPR_BINARITHM:
		check_expr_binarithm(ctx, aexpr, expr);
		break;
	case EXPR_BINDING:
		check_expr_binding(ctx, aexpr, expr);
		break;
	case EXPR_BREAK:
		assert(0); // TODO
	case EXPR_CALL:
		check_expr_call(ctx, aexpr, expr);
		break;
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		check_expr_constant(ctx, aexpr, expr);
		break;
	case EXPR_CONTINUE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
		assert(0); // TODO
	case EXPR_LIST:
		check_expr_list(ctx, aexpr, expr);
		break;
	case EXPR_MATCH:
		assert(0); // TODO
	case EXPR_MEASURE:
		check_expr_measure(ctx, aexpr, expr);
		break;
	case EXPR_RETURN:
		check_expr_return(ctx, aexpr, expr);
		break;
	case EXPR_SLICE:
	case EXPR_STRUCT:
	case EXPR_SWITCH:
		assert(0); // TODO
	case EXPR_UNARITHM:
		check_expr_unarithm(ctx, aexpr, expr);
		break;
	case EXPR_WHILE:
		assert(0); // TODO
	}

	trleave(TR_CHECK, NULL);
}

static struct declaration *
check_function(struct context *ctx,
	const struct ast_decl *adecl)
{
	if (!adecl->function.body) {
		return NULL; // Prototype
	}

	const struct ast_function_decl *afndecl = &adecl->function;
	trenter(TR_CHECK, "function");
	assert(!afndecl->symbol); // TODO

	const struct ast_type fn_atype = {
		.storage = TYPE_STORAGE_FUNCTION,
		.flags = TYPE_CONST,
		.func = afndecl->prototype,
	};
	const struct type *fntype = type_store_lookup_atype(
			&ctx->store, &fn_atype);
	assert(fntype); // Invariant

	struct declaration *decl = calloc(1, sizeof(struct declaration));
	decl->type = DECL_FUNC;
	decl->func.type = fntype;
	// TODO: Rewrite ident to be a member of the unit's namespace
	identifier_dup(&decl->ident, &afndecl->ident);
	decl->func.flags = afndecl->flags;

	decl->func.scope = scope_push(&ctx->scope, TR_CHECK);
	struct ast_function_parameters *params = afndecl->prototype.params;
	while (params) {
		struct identifier ident = {
			.name = params->name,
		};
		const struct type *type = type_store_lookup_atype(
				&ctx->store, params->type);
		scope_insert(decl->func.scope, O_BIND, &ident, type);
		params = params->next;
	}

	struct expression *body = calloc(1, sizeof(struct expression));
	check_expression(ctx, afndecl->body, body);
	decl->func.body = body;

	// TODO: Check assignability of expression result to function type

	// TODO: Add function name to errors
	if ((decl->func.flags & FN_INIT)
			|| (decl->func.flags & FN_FINI)
			|| (decl->func.flags & FN_TEST)) {
		const char *flags = "@flags"; // TODO: Unparse flags
		expect(fntype->func.result == &builtin_type_void,
				"%s function must return void", flags);
		expect(!decl->exported,
				"%s function cannot be exported", flags);
	}

	scope_insert(ctx->unit, O_DECL, &decl->ident, decl->func.type);
	scope_pop(&ctx->scope, TR_CHECK);
	trleave(TR_CHECK, NULL);
	return decl;
}

static void
check_declarations(struct context *ctx,
		const struct ast_decls *adecls,
		struct declarations **next)
{
	trenter(TR_CHECK, "declarations");
	while (adecls) {
		struct declaration *decl;
		const struct ast_decl *adecl = &adecls->decl;
		switch (adecl->decl_type) {
		case AST_DECL_FUNC:
			decl = check_function(ctx, adecl);
			break;
		case AST_DECL_TYPE:
			assert(0); // TODO
		case AST_DECL_GLOBAL:
			assert(0); // TODO
		case AST_DECL_CONST:
			assert(0); // TODO
		}

		if (decl) {
			struct declarations *decls = *next =
				calloc(1, sizeof(struct declarations));
			decl->exported = adecl->exported;
			decls->decl = decl;
			next = &decls->next;
		}

		adecls = adecls->next;
	}
	trleave(TR_CHECK, NULL);
}

static void
scan_function(struct context *ctx, const struct ast_function_decl *decl)
{
	trenter(TR_SCAN, "function");
	const struct ast_type fn_atype = {
		.storage = TYPE_STORAGE_FUNCTION,
		.flags = TYPE_CONST,
		.func = decl->prototype,
	};
	const struct type *fntype = type_store_lookup_atype(
			&ctx->store, &fn_atype);
	assert(fntype); // TODO: Forward references

	char buf[1024];
	identifier_unparse_static(&decl->ident, buf, sizeof(buf));
	trleave(TR_SCAN, "func %s", buf);

	if (!decl->body) {
		scope_insert(ctx->unit, O_DECL, &decl->ident, fntype);
	}
}

static void
scan_declarations(struct context *ctx, const struct ast_decls *decls)
{
	trenter(TR_SCAN, "declarations");
	while (decls) {
		const struct ast_decl *decl = &decls->decl;
		switch (decl->decl_type) {
		case AST_DECL_FUNC:
			scan_function(ctx, &decl->function);
			break;
		case AST_DECL_TYPE:
			assert(0); // TODO
		case AST_DECL_GLOBAL:
			assert(0); // TODO
		case AST_DECL_CONST:
			assert(0); // TODO
		}
		decls = decls->next;
	}
	trleave(TR_SCAN, NULL);
}

void
check(const struct ast_unit *aunit, struct unit *unit)
{
	struct context ctx = {0};

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

	// First pass populates the type graph
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		scope_push(&ctx.scope, TR_SCAN);

		assert(!su->imports); // TODO
		scan_declarations(&ctx, &su->decls);

		*next = calloc(1, sizeof(struct scopes));
		(*next)->scope = scope_pop(&ctx.scope, TR_SCAN);
		next = &(*next)->next;
	}

	// Second pass populates the expression graph
	struct scopes *scope = subunit_scopes;
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		ctx.scope = scope->scope;
		trenter(TR_CHECK, "scope %p", ctx.scope);
		check_declarations(&ctx, &su->decls, &unit->declarations);
		trleave(TR_CHECK, NULL);
		scope = scope->next;
	}

	assert(unit->declarations);
}
