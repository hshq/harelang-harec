#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"
#include "check.h"
#include "expr.h"
#include "trace.h"
#include "type_store.h"
#include "types.h"

struct context {
	struct type_store store;
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
	}
}

static void check_expression(struct context *ctx,
	const struct ast_expression *aexpr, struct expression *expr);

static void
check_expr_constant(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trace(TR_CHECK, "constant");
	expr->type = EXPR_CONSTANT;
	expr->result = builtin_type_for_storage(aexpr->constant.storage, true);

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

	struct expression_list *list = &expr->list;
	struct expression_list **next = &list->next;

	const struct ast_expression_list *alist = &aexpr->list;
	while (alist) {
		struct expression *lexpr = calloc(1, sizeof(struct expression));
		check_expression(ctx, alist->expr, lexpr);
		list->expr = lexpr;

		alist = alist->next;
		if (alist) {
			*next = calloc(1, sizeof(struct expression_list));
			list = *next;
			next = &list->next;
		} else {
			expr->result = lexpr->result;
		}
	}

	trleave(TR_CHECK, NULL);
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
check_expression(struct context *ctx,
	const struct ast_expression *aexpr,
	struct expression *expr)
{
	trenter(TR_CHECK, "expression");

	switch (aexpr->type) {
	case EXPR_ACCESS:
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINARITHM:
	case EXPR_BINDING_LIST:
	case EXPR_BREAK:
	case EXPR_CALL:
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		check_expr_constant(ctx, aexpr, expr);
		break;
	case EXPR_CONTINUE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_FUNC:
	case EXPR_IF:
	case EXPR_INDEX:
		assert(0); // TODO
	case EXPR_LIST:
		check_expr_list(ctx, aexpr, expr);
		break;
	case EXPR_MATCH:
	case EXPR_MEASURE:
	case EXPR_RETURN:
		check_expr_return(ctx, aexpr, expr);
		break;
	case EXPR_SLICE:
	case EXPR_STRUCT:
	case EXPR_SWITCH:
	case EXPR_UNARITHM:
	case EXPR_WHILE:
		assert(0); // TODO
	}

	trleave(TR_CHECK, NULL);
}

static void
check_function(struct context *ctx,
	const struct ast_decl *adecl,
	struct declaration *decl)
{
	const struct ast_function_decl *afndecl = &adecl->function;
	trenter(TR_CHECK, "function");
	assert(!afndecl->prototype.params); // TODO
	assert(!afndecl->symbol); // TODO

	const struct ast_type fn_atype = {
		.storage = TYPE_STORAGE_FUNCTION,
		.flags = TYPE_CONST,
		.func = afndecl->prototype,
	};
	const struct type *fntype = type_store_lookup_atype(
			&ctx->store, &fn_atype);
	assert(fntype); // Invariant
	decl->type = DECL_FUNC;
	decl->func.type = fntype;
	// TODO: Rewrite ident to be a member of the unit's namespace
	identifier_dup(&decl->ident, &afndecl->ident);
	decl->func.flags = afndecl->flags;

	struct expression *body = calloc(1, sizeof(struct expression));
	check_expression(ctx, &afndecl->body, body);
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
	trleave(TR_CHECK, NULL);
}

static void
check_declarations(struct context *ctx,
		const struct ast_decls *adecls,
		struct declarations **next)
{
	trenter(TR_CHECK, "declarations");
	while (adecls) {
		struct declarations *decls = *next =
			calloc(1, sizeof(struct declarations));
		struct declaration *decl = &decls->decl;
		const struct ast_decl *adecl = &adecls->decl;
		decl->exported = adecl->exported;
		switch (adecl->decl_type) {
		case AST_DECL_FUNC:
			check_function(ctx, adecl, decl);
			break;
		case AST_DECL_TYPE:
			assert(0); // TODO
		case AST_DECL_GLOBAL:
			assert(0); // TODO
		case AST_DECL_CONST:
			assert(0); // TODO
		}
		adecls = adecls->next;
		next = &decls->next;
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
	// First pass populates the type graph
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		scan_declarations(&ctx, &su->decls);
	}

	// Second pass populates the expression graph
	for (const struct ast_subunit *su = &aunit->subunits;
			su; su = su->next) {
		check_declarations(&ctx, &su->decls, &unit->declarations);
	}

	assert(unit->declarations);
}
