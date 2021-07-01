#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "check.h"
#include "expr.h"
#include "gen.h"
#include "scope.h"
#include "types.h"
#include "util.h"

static char *
gen_name(struct gen_context *ctx, const char *fmt)
{
	int n = snprintf(NULL, 0, fmt, ctx->id);
	char *str = xcalloc(1, n + 1);
	snprintf(str, n + 1, fmt, ctx->id);
	++ctx->id;
	return str;
}

// Initializes a qval with a reference to a gen temporary.
static void
qval_temp(struct gen_context *ctx,
		struct qbe_value *out,
		const struct gen_temp *temp)
{
	out->kind = QV_TEMPORARY;
	out->type = qtype_lookup(ctx, temp->type);
	out->name = temp->name;
}

// Allocates a temporary of the given type on the stack in this function's
// preamble.
static struct gen_temp *
alloc_temp(struct gen_context *ctx, const struct type *type, const char *fmt)
{
	assert(type->size != 0 && type->size != SIZE_UNDEFINED);

	struct gen_temp *temp = xcalloc(1, sizeof(struct gen_temp));
	temp->type = type;
	temp->name = gen_name(ctx, fmt);

	struct qbe_value out = {
		.kind = QV_TEMPORARY,
		.type = ctx->arch.ptr,
		.name = temp->name,
	};
	struct qbe_value size;
	constl(&size, type->size);
	pushprei(ctx->current, &out, alloc_for_align(type->align), &size, NULL);
	return temp;
}

// Loads a gen temporary into a qbe temporary. For types representable in qbe's
// type system, this loads the actual value into a qbe temporary. Otherwise,
// this behaves equivalently to qval_temp, but sets the temporary type to the
// platform's pointer type (e.g. =l).
static void
load_temp(struct gen_context *ctx,
	struct qbe_value *out,
	const struct gen_temp *temp)
{
	const struct qbe_type *qtype = qtype_lookup(ctx, temp->type);
	assert(qtype->stype != Q__VOID);

	out->kind = QV_TEMPORARY;
	if (qtype->stype == Q__AGGREGATE) {
		out->name = temp->name;
		out->type = ctx->arch.ptr;
	} else {
		out->name = gen_name(ctx, "load.%d");
		out->type = qtype;

		struct qbe_value addr;
		qval_temp(ctx, &addr, temp);
		enum qbe_instr instr = load_for_type(temp->type);
		pushi(ctx->current, out, instr, &addr, NULL);
	}
}

static void
gen_expr_constant(struct gen_context *ctx,
		const struct expression *expr,
		struct gen_temp *out)
{
	if (out == NULL) {
		pushc(ctx->current, "Useless constant expression dropped");
		return;
	}
	const struct expression_constant *constexpr = &expr->constant;
	assert(constexpr->object == NULL); // TODO

	struct qbe_value qout, qval = {0};
	qval_temp(ctx, &qout, out);

	switch (type_dealias(expr->result)->storage) {
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
		constw(&qval, constexpr->uval);
		break;
	case STORAGE_I16:
	case STORAGE_U16:
		constw(&qval, constexpr->uval);
		break;
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_RUNE:
	case STORAGE_BOOL:
		constw(&qval, constexpr->uval);
		break;
	case STORAGE_I64:
	case STORAGE_U64:
	case STORAGE_SIZE:
	case STORAGE_UINTPTR:
		constl(&qval, constexpr->uval);
		break;
	case STORAGE_POINTER:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_ENUM:
		assert(0); // TODO
	case STORAGE_ARRAY:
	case STORAGE_NULL:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
		assert(0); // TODO
	case STORAGE_ICONST:
	case STORAGE_FCONST:
	case STORAGE_VOID:
	case STORAGE_ALIAS:
	case STORAGE_FUNCTION:
		abort(); // Invariant
	}

	enum qbe_instr instr = store_for_type(expr->result);
	pushi(ctx->current, NULL, instr, &qval, &qout, NULL);
}

static void
gen_expr(struct gen_context *ctx,
		const struct expression *expr,
		struct gen_temp *out)
{
	switch (expr->type) {
	case EXPR_ACCESS:
	case EXPR_ALLOC:
	case EXPR_APPEND:
	case EXPR_ASSERT:
	case EXPR_ASSIGN:
	case EXPR_BINARITHM:
	case EXPR_BINDING:
	case EXPR_BREAK:
	case EXPR_CONTINUE:
	case EXPR_CALL:
	case EXPR_CAST:
		assert(0); // TODO
	case EXPR_CONSTANT:
		gen_expr_constant(ctx, expr, out);
		break;
	case EXPR_DEFER:
	case EXPR_DELETE:
	case EXPR_FOR:
	case EXPR_FREE:
	case EXPR_IF:
	case EXPR_INSERT:
	case EXPR_LIST:
	case EXPR_MATCH:
	case EXPR_MEASURE:
		assert(0); // TODO
	case EXPR_PROPAGATE:
		assert(0); // Lowered in check (XXX: for now...)
	case EXPR_RETURN:
	case EXPR_SLICE:
	case EXPR_STRUCT:
	case EXPR_SWITCH:
	case EXPR_TUPLE:
	case EXPR_UNARITHM:
		assert(0); // TODO
	}
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
		ctx->rval = alloc_temp(ctx, fntype->func.result, "rval.%d");
		qdef->func.returns = qtype_lookup(ctx, fntype->func.result);
	} else {
		qdef->func.returns = &qbe_void;
	}

	// TODO: Allocate parameters
	assert(!func->scope->objects);

	pushl(&qdef->func, &ctx->id, "body.%d");
	gen_expr(ctx, func->body, ctx->rval);

	pushl(&qdef->func, &ctx->id, "end.%d");
	if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		struct qbe_value rval = {0};
		load_temp(ctx, &rval, ctx->rval);
		pushi(ctx->current, NULL, Q_RET, &rval, NULL);
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
		},
	};
	ctx.out->next = &ctx.out->defs;
	const struct declarations *decls = unit->declarations;
	while (decls) {
		gen_decl(&ctx, decls->decl);
		decls = decls->next;
	}
}
