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
#include "trace.h"
#include "types.h"

struct gen_context {
	struct qbe_program *out;
	struct identifier *ns;
	uint64_t id;
};

static char *
ident_to_sym(const struct identifier *ident)
{
	if (ident->ns) {
		char *ns = ident_to_sym(ident->ns);
		if (!ns) {
			return NULL;
		}
		int n = snprintf(NULL, 0, "%s.%s", ns, ident->name);
		char *str = calloc(1, n + 1);
		assert(str);
		snprintf(str, n + 1, "%s.%s", ns, ident->name);
		free(ns);
		return str;
	}
	return strdup(ident->name);
}

static enum qbe_stype
qstype_for_type(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_U8:
		// Implemented as Q_WORD
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
		// Implemented as Q_WORD
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_INT:		// XXX: Architecture dependent
	case TYPE_STORAGE_UINT:		// XXX: Architecture dependent
		return Q_WORD;
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_UINTPTR:	// XXX: Architecture dependent
	case TYPE_STORAGE_POINTER:	// XXX: Architecture dependent
		return Q_LONG;
	case TYPE_STORAGE_F32:
		return Q_SINGLE;
	case TYPE_STORAGE_F64:
		return Q_DOUBLE;
	case TYPE_STORAGE_VOID:
		return Q__VOID;
	case TYPE_STORAGE_ALIAS:
		assert(0); // TODO
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_FUNCTION:
		assert(0); // Invariant
	}
	assert(0);
}

static enum qbe_stype
qxtype_for_type(const struct type *type)
{
	switch (type->storage) {
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_U8:
		return Q_BYTE;
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
		return Q_HALF;
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_INT:		// XXX: Architecture dependent
	case TYPE_STORAGE_UINT:		// XXX: Architecture dependent
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_UINTPTR:	// XXX: Architecture dependent
	case TYPE_STORAGE_POINTER:	// XXX: Architecture dependent
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
	case TYPE_STORAGE_FUNCTION:
		return qstype_for_type(type);
	}
	assert(0);
}

static const struct qbe_type *
qtype_for_type(struct gen_context *ctx, const struct type *type, bool extended)
{
	switch (type->storage) {
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_U16:
		if (extended) {
			return qtype_for_xtype(qxtype_for_type(type));
		}
		// Fallthrough
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_VOID:
		return qtype_for_xtype(qstype_for_type(type));
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	case TYPE_STORAGE_FUNCTION:
		assert(0); // Invariant
	}
	assert(0); // Unreachable
}

static void
gen_function_decl(struct gen_context *ctx, const struct declaration *decl)
{
	assert(decl->type == DECL_FUNC);
	const struct function_decl *func = &decl->func;
	const struct type *fntype = func->type;
	assert(func->flags == 0); // TODO

	struct qbe_def *qdef = calloc(1, sizeof(struct qbe_def));
	qdef->type = Q_FUNC;
	qdef->exported = decl->exported;
	qdef->name = func->symbol ? strdup(func->symbol)
		: ident_to_sym(&decl->ident);
	qdef->func.returns = qtype_for_type(ctx, fntype->func.result, true);

	assert(fntype->func.params == NULL); // TODO

	// TODO: Gen function body
}

static void
gen_decl(struct gen_context *ctx, const struct declaration *decl)
{
	switch (decl->type) {
	case DECL_FUNC:
		gen_function_decl(ctx, decl);
		break;
	case DECL_TYPE:
	case DECL_GLOBAL:
	case DECL_CONSTANT:
		assert(0); // TODO
	}
}

void
gen(const struct unit *unit, struct qbe_program *out)
{
	struct gen_context ctx = {
		.out = out,
		.ns = unit->ns,
	};
	const struct declarations *decls = unit->declarations;
	assert(decls); // At least one is required
	trenter(TR_GEN, "gen");
	while (decls) {
		gen_decl(&ctx, &decls->decl);
		decls = decls->next;
	}
	trleave(TR_GEN, NULL);
}
