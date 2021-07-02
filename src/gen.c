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

// Initializes a qval with a qbe temporary for a given gen temporary.
static void
qval_temp(struct gen_context *ctx,
	struct qbe_value *out,
	const struct gen_temp *temp)
{
	out->kind = QV_TEMPORARY;
	out->type = qtype_lookup(ctx, temp->type, true);
	out->name = temp->name;
}

// Initializes a qbe_value as a qbe temporary for the given qbe type.
static void
gen_qtemp(struct gen_context *ctx, struct qbe_value *out,
		const struct qbe_type *type, const char *fmt)
{
	out->kind = QV_TEMPORARY;
	out->type = type;
	out->name = gen_name(ctx, fmt);
}

// Allocates a temporary of the given type on the stack in the current
// function's preamble.
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
	const struct qbe_type *qtype = qtype_lookup(ctx, temp->type, true);
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
		enum qbe_instr instr = load_for_type(ctx, temp->type);
		pushi(ctx->current, out, instr, &addr, NULL);
	}
}

static void gen_expr(struct gen_context *ctx,
	const struct expression *expr, struct gen_temp *out);

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

	enum type_storage storage = type_dealias(expr->result)->storage;
	if (storage == STORAGE_ENUM) {
		storage = type_dealias(expr->result)->_enum.storage;
	}

	switch (storage) {
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
	case STORAGE_I16:
	case STORAGE_U16:
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
		constl(&qval, constexpr->uval);
		break;
	case STORAGE_F32:
		consts(&qval, constexpr->fval);
		break;
	case STORAGE_F64:
		constd(&qval, constexpr->fval);
		break;
	case STORAGE_SIZE:
		switch (ctx->arch.sz->size) {
		case 8:
			constl(&qval, constexpr->uval);
			break;
		default:
			abort();
		}
		break;
	case STORAGE_UINTPTR:
	case STORAGE_POINTER:
	case STORAGE_ARRAY:
	case STORAGE_NULL:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
		assert(0); // TODO
	case STORAGE_ICONST:
	case STORAGE_FCONST:
	case STORAGE_ENUM:
	case STORAGE_VOID:
	case STORAGE_ALIAS:
	case STORAGE_FUNCTION:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		abort(); // Invariant
	}

	enum qbe_instr instr = store_for_type(ctx, expr->result);
	pushi(ctx->current, NULL, instr, &qval, &qout, NULL);
}

static void
gen_expr_list(struct gen_context *ctx,
		const struct expression *expr,
		struct gen_temp *out)
{
	for (const struct expressions *item = &expr->list.exprs;
			item; item = item->next) {
		if (!item->next) {
			gen_expr(ctx, item->expr, out);
		} else {
			gen_expr(ctx, item->expr, NULL);
		}
	}
}

static void
gen_expr_return(struct gen_context *ctx,
		const struct expression *expr,
		struct gen_temp *out)
{
	struct qbe_value label = {
		.kind = QV_LABEL,
		.name = strdup(ctx->end),
	};
	if (expr->_return.value) {
		gen_expr(ctx, expr->_return.value, ctx->rval);
	}
	pushi(ctx->current, NULL, Q_JMP, &label, NULL);
}

static void
gen_expr_struct(struct gen_context *ctx,
		const struct expression *expr,
		struct gen_temp *out)
{
	if (!out) {
		pushc(ctx->current, "Useless struct expression dropped");
		return;
	}
	struct qbe_value base = {0}, ptr = {0}, offs = {0};
	qval_temp(ctx, &base, out);
	gen_qtemp(ctx, &ptr, ctx->arch.ptr, "offset.%d");

	if (expr->_struct.autofill) {
		struct qbe_value rtfunc = {0}, size = {0}, zero = {0};
		rtfunc.kind = QV_GLOBAL;
		rtfunc.name = strdup("rt.memset");
		rtfunc.type = &qbe_long;
		constl(&size, expr->result->size);
		constl(&zero, 0);
		pushi(ctx->current, NULL, Q_CALL, &rtfunc,
				&base, &zero, &size, NULL);
	}

	const struct expr_struct_field *field = &expr->_struct.fields;
	while (field) {
		if (!field->value) {
			assert(expr->_struct.autofill);
			field = field->next;
			continue;
		}

		constl(&offs, field->field->offset);
		pushi(ctx->current, &ptr, Q_ADD, &base, &offs, NULL);

		struct gen_temp temp = {
			.name = ptr.name,
			.type = field->field->type,
		};
		gen_expr(ctx, field->value, &temp);
		field = field->next;
	}
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
		assert(0); // TODO
	case EXPR_LIST:
		gen_expr_list(ctx, expr, out);
		break;
	case EXPR_MATCH:
	case EXPR_MEASURE:
		assert(0); // TODO
	case EXPR_PROPAGATE:
		assert(0); // Lowered in check (XXX: for now...)
	case EXPR_RETURN:
		gen_expr_return(ctx, expr, out);
		break;
	case EXPR_SLICE:
		assert(0); // TODO
	case EXPR_STRUCT:
		gen_expr_struct(ctx, expr, out);
		break;
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
	struct qbe_statement end_label = {0};
	genl(&start_label, &ctx->id, "start.%d");
	ctx->end = genl(&end_label, &ctx->id, "end.%d");
	push(&qdef->func.prelude, &start_label);

	if (type_dealias(fntype->func.result)->storage != STORAGE_VOID) {
		ctx->rval = alloc_temp(ctx, fntype->func.result, "rval.%d");
		qdef->func.returns = qtype_lookup(ctx, fntype->func.result, true);
	} else {
		qdef->func.returns = &qbe_void;
	}

	// TODO: Allocate parameters
	assert(!func->scope->objects);

	pushl(&qdef->func, &ctx->id, "body.%d");
	gen_expr(ctx, func->body, ctx->rval);

	push(&qdef->func.body, &end_label);
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
