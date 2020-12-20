#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "qbe.h"

static void
emit_qtype(const struct qbe_type *type, FILE *out)
{
	switch (type->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
	case Q_LONG:
	case Q_SINGLE:
	case Q_DOUBLE:
		fprintf(out, "%c ", (char)type->stype);
		break;
	case Q__VOID:
		break; // no-op
	case Q__AGGREGATE:
		assert(0); // TODO
	}
}

static void
emit_func(struct qbe_def *def, FILE *out)
{
	assert(def->type == Q_FUNC);
	// TODO: Parameters
	fprintf(out, "%sfunction ", def->exported ? "export " : "");
	emit_qtype(def->func.returns, out);
	fprintf(out, "$%s() {\n", def->name); // TODO: Parameters
	// TODO: Body
	fprintf(out, "}\n\n");
}

static void
emit_def(struct qbe_def *def, FILE *out)
{
	switch (def->type) {
	case Q_TYPE:
		assert(0); // TODO
	case Q_FUNC:
		emit_func(def, out);
		break;
	case Q_DATA:
		assert(0); // TODO
	}
}

void
emit(struct qbe_program *program, FILE *out)
{
	struct qbe_def *def = program->defs;
	assert(def); // At least one
	while (def) {
		emit_def(def, out);
		def = def->next;
	}
}
