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
		fprintf(out, "%c", (char)type->stype);
		break;
	case Q__VOID:
		break; // no-op
	case Q__AGGREGATE:
		fprintf(out, "l"); // XXX: ARCH
		break;
	}
}

static void
emit_const(struct qbe_value *val, FILE *out)
{
	switch (val->type->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
		fprintf(out, "%u", val->wval);
		break;
	case Q_LONG:
		fprintf(out, "%lu", val->lval);
		break;
	case Q_SINGLE:
		fprintf(out, "%f", val->sval);
		break;
	case Q_DOUBLE:
		fprintf(out, "%f", val->dval);
		break;
	case Q__VOID:
	case Q__AGGREGATE:
		assert(0); // Invariant
	}
}

static void
emit_value(struct qbe_value *val, FILE *out)
{
	switch (val->kind) {
	case QV_CONST:
		emit_const(val, out);
		break;
	case QV_GLOBAL:
		fprintf(out, "$%s", val->name);
		break;
	case QV_LABEL:
		fprintf(out, "@%s", val->name);
		break;
	case QV_TEMPORARY:
		fprintf(out, "%%%s", val->name);
		break;
	}
}

static void
emit_call(struct qbe_statement *stmt, FILE *out)
{
	fprintf(out, "%s ", qbe_instr[stmt->instr]);

	struct qbe_arguments *arg = stmt->args;
	assert(arg);
	emit_value(&arg->value, out);
	fprintf(out, "(");
	arg = arg->next;

	bool comma = false;
	while (arg) {
		fprintf(out, "%s", comma ? ", " : "");
		emit_qtype(arg->value.type, out);
		fprintf(out, " ");
		emit_value(&arg->value, out);
		arg = arg->next;
		comma = true;
	}

	fprintf(out, ")\n");
}

static void
emit_stmt(struct qbe_statement *stmt, FILE *out)
{
	switch (stmt->type) {
	case Q_COMMENT:
		fprintf(out, "\t# %s\n", stmt->comment);
		break;
	case Q_INSTR:
		fprintf(out, "\t");
		if (stmt->out != NULL) {
			emit_value(stmt->out, out);
			fprintf(out, " =");
			if (stmt->out->indirect) {
				emit_qtype(&qbe_long, out); // XXX: ARCH
			} else {
				emit_qtype(stmt->out->type, out);
			}
			fprintf(out, " ");
		}
		if (stmt->instr == Q_CALL) {
			emit_call(stmt, out);
			break;
		}
		fprintf(out, "%s%s", qbe_instr[stmt->instr],
				stmt->args ? " " : "");
		struct qbe_arguments *arg = stmt->args;
		while (arg) {
			fprintf(out, "%s", arg == stmt->args ? "" : ", ");
			emit_value(&arg->value, out);
			arg = arg->next;
		}
		fprintf(out, "\n");
		break;
	case Q_LABEL:
		fprintf(out, "@%s\n", stmt->label);
		break;
	}
}

static void
emit_func(struct qbe_def *def, FILE *out)
{
	assert(def->type == Q_FUNC);
	fprintf(out, "%sfunction ", def->exported ? "export " : "");
	emit_qtype(def->func.returns, out);
	fprintf(out, " $%s(", def->name);
	struct qbe_func_param *param = def->func.params;
	while (param) {
		emit_qtype(param->type, out);
		fprintf(out, " %%%s", param->name);
		if (param->next) {
			fprintf(out, ", ");
		}
		param = param->next;
	}
	fprintf(out, ") {\n");

	for (size_t i = 0; i < def->func.blen; ++i) {
		struct qbe_statement *stmt = &def->func.body[i];
		emit_stmt(stmt, out);
	}

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
