#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include "emit.h"
#include "qbe.h"

static void
emit_qtype(const struct qbe_type *type, bool aggr, FILE *out)
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
	case Q__AGGREGATE:
		if (aggr) {
			fprintf(out, ":%s", type->name);
		} else {
			fprintf(out, "l");
		}
		break;
	case Q__VOID:
		break; // no-op
	}
}

static void
emit_type(const struct qbe_def *def, FILE *out)
{
	assert(def->kind == Q_TYPE);
	fprintf(out, "type :%s =", def->name);
	if (def->type.align != (size_t)-1) {
		fprintf(out, " align %zu", def->type.align);
	}
	fprintf(out, " {");

	const struct qbe_field *field = &def->type.fields;
	while (field) {
		if (def->type.is_union) {
			fprintf(out, " {");
		}
		if (field->type) {
			fprintf(out, " ");
			emit_qtype(field->type, true, out);
		}
		if (field->count) {
			fprintf(out, " %zu", field->count);
		}
		if (def->type.is_union) {
			fprintf(out, " }");
		} else if (field->next) {
			fprintf(out, ",");
		}
		field = field->next;
	}

	fprintf(out, " }\n\n");
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
		emit_qtype(arg->value.type, true, out);
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
			emit_qtype(stmt->out->type, false, out);
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
	assert(def->kind == Q_FUNC);
	fprintf(out, "%sfunction", def->exported ? "export " : "");
	if (def->func.returns->stype != Q__VOID) {
		fprintf(out, " ");
		emit_qtype(def->func.returns, true, out);
	}
	fprintf(out, " $%s(", def->name);
	struct qbe_func_param *param = def->func.params;
	while (param) {
		emit_qtype(param->type, true, out);
		fprintf(out, " %%%s", param->name);
		if (param->next) {
			fprintf(out, ", ");
		}
		param = param->next;
	}
	fprintf(out, ") {\n");

	for (size_t i = 0; i < def->func.prelude.ln; ++i) {
		struct qbe_statement *stmt = &def->func.prelude.stmts[i];
		emit_stmt(stmt, out);
	}

	for (size_t i = 0; i < def->func.body.ln; ++i) {
		struct qbe_statement *stmt = &def->func.body.stmts[i];
		emit_stmt(stmt, out);
	}

	fprintf(out, "}\n\n");
}

static void
emit_data_string(const char *str, size_t sz, FILE *out)
{
	bool q = false;
	for (size_t i = 0; i < sz; ++i) {
		/* XXX: We could stand to emit less conservatively */
		if (!isprint(str[i]) || str[i] == '"' || str[i] == '\\') {
			if (q) {
				q = false;
				fprintf(out, "\", ");
			}
			fprintf(out, "b %d, ", str[i]);
		} else {
			if (!q) {
				q = true;
				fprintf(out, "b \"");
			}
			fprintf(out, "%c", str[i]);
		}
	}
	if (q) {
		fprintf(out, "\", b 0");
	} else {
		fprintf(out, "b 0");
	}
}

static void
emit_data(struct qbe_def *def, FILE *out)
{
	assert(def->kind == Q_DATA);
	fprintf(out, "%sdata ", def->exported ? "export " : "");
	if (def->data.section) {
		// XXX: Presumes section name does not need to be escaped
		fprintf(out, "section \"%s\" ", def->data.section);
	}
	fprintf(out, "$%s = { ", def->name);

	struct qbe_data_item *item = &def->data.items;
	while (item) {
		switch (item->type) {
		case QD_VALUE:
			emit_qtype(item->value.type, true, out);
			fprintf(out, " ");
			emit_value(&item->value, out);
			break;
		case QD_ZEROED:
			fprintf(out, "z %zu", item->zeroed);
			break;
		case QD_STRING:
			emit_data_string(item->str, item->sz, out);
			break;
		}

		fprintf(out, item->next ? ", " : " ");
		item = item->next;
	}

	fprintf(out, "}\n\n");
}

static void
emit_def(struct qbe_def *def, FILE *out)
{
	switch (def->kind) {
	case Q_TYPE:
		emit_type(def, out);
		break;
	case Q_FUNC:
		emit_func(def, out);
		break;
	case Q_DATA:
		emit_data(def, out);
		break;
	}
}

void
emit(struct qbe_program *program, FILE *out)
{
	struct qbe_def *def = program->defs;
	while (def) {
		emit_def(def, out);
		def = def->next;
	}
}
