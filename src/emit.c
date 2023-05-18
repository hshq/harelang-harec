#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include "emit.h"
#include "qbe.h"
#include "typedef.h"
#include "types.h"
#include "util.h"

static void
emit_qtype(const struct qbe_type *type, bool aggr, FILE *out)
{
	assert(type);
	switch (type->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
	case Q_LONG:
	case Q_SINGLE:
	case Q_DOUBLE:
		xfprintf(out, "%c", (char)type->stype);
		break;
	case Q__AGGREGATE:
		if (aggr) {
			xfprintf(out, ":%s", type->name);
		} else {
			xfprintf(out, "l");
		}
		break;
	case Q__VOID:
		break; // no-op
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
qemit_type(const struct qbe_def *def, FILE *out)
{
	assert(def->kind == Q_TYPE);
	const struct type *base = def->type.base;
	if (base) {
		char *tn = gen_typename(base);
		xfprintf(out, "# %s [id: %u; size: %zu]\n", tn, base->id, base->size);
		free(tn);
		xfprintf(out, "type :%s =", def->name);
		if (base->align != (size_t)-1) {
			xfprintf(out, " align %zu", base->align);
		}
	} else {
		xfprintf(out, "type :%s =", def->name);
	}
	xfprintf(out, " {");

	bool is_union = base == NULL || type_dealias(base)->storage == STORAGE_UNION;
	const struct qbe_field *field = &def->type.fields;
	while (field) {
		if (is_union) {
			xfprintf(out, " {");
		}
		if (field->type) {
			xfprintf(out, " ");
			emit_qtype(field->type, true, out);
		}
		if (field->count) {
			xfprintf(out, " %zu", field->count);
		}
		if (is_union) {
			xfprintf(out, " }");
		} else if (field->next) {
			xfprintf(out, ",");
		}
		field = field->next;
	}

	xfprintf(out, " }\n\n");
}

static void
emit_const(struct qbe_value *val, FILE *out)
{
	switch (val->type->stype) {
	case Q_BYTE:
	case Q_HALF:
	case Q_WORD:
	case Q_SINGLE:
		xfprintf(out, "%" PRIu32, val->wval);
		break;
	case Q_LONG:
	case Q_DOUBLE:
		xfprintf(out, "%" PRIu64, val->lval);
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
		if (val->threadlocal) {
			xfprintf(out, "thread ");
		}
		xfprintf(out, "$%s", val->name);
		break;
	case QV_LABEL:
		xfprintf(out, "@%s", val->name);
		break;
	case QV_TEMPORARY:
		xfprintf(out, "%%%s", val->name);
		break;
	case QV_VARIADIC:
		xfprintf(out, "...");
		break;
	}
}

static void
emit_call(struct qbe_statement *stmt, FILE *out)
{
	xfprintf(out, "%s ", qbe_instr[stmt->instr]);

	struct qbe_arguments *arg = stmt->args;
	assert(arg);
	emit_value(&arg->value, out);
	xfprintf(out, "(");
	arg = arg->next;

	bool comma = false;
	while (arg) {
		xfprintf(out, "%s", comma ? ", " : "");
		if (arg->value.kind != QV_VARIADIC) {
			emit_qtype(arg->value.type, true, out);
			xfprintf(out, " ");
		}
		emit_value(&arg->value, out);
		arg = arg->next;
		comma = true;
	}

	xfprintf(out, ")\n");
}

static void
emit_stmt(struct qbe_statement *stmt, FILE *out)
{
	switch (stmt->type) {
	case Q_COMMENT:
		xfprintf(out, "\t# %s\n", stmt->comment);
		break;
	case Q_INSTR:
		xfprintf(out, "\t");
		if (stmt->instr == Q_CALL) {
			if (stmt->out != NULL) {
				emit_value(stmt->out, out);
				xfprintf(out, " =");
				emit_qtype(stmt->out->type, true, out);
				xfprintf(out, " ");
			}
			emit_call(stmt, out);
			break;
		}
		if (stmt->out != NULL) {
			emit_value(stmt->out, out);
			xfprintf(out, " =");
			emit_qtype(stmt->out->type, false, out);
			xfprintf(out, " ");
		}
		xfprintf(out, "%s%s", qbe_instr[stmt->instr],
				stmt->args ? " " : "");
		struct qbe_arguments *arg = stmt->args;
		while (arg) {
			xfprintf(out, "%s", arg == stmt->args ? "" : ", ");
			emit_value(&arg->value, out);
			arg = arg->next;
		}
		xfprintf(out, "\n");
		break;
	case Q_LABEL:
		xfprintf(out, "@%s\n", stmt->label);
		break;
	}
}

static void
emit_func(struct qbe_def *def, FILE *out)
{
	assert(def->kind == Q_FUNC);
	xfprintf(out, "section \".text.%s\" \"ax\"%s\nfunction",
			def->name,
			def->exported ? " export" : "");
	if (def->func.returns->stype != Q__VOID) {
		xfprintf(out, " ");
		emit_qtype(def->func.returns, true, out);
	}
	xfprintf(out, " $%s(", def->name);
	struct qbe_func_param *param = def->func.params;
	while (param) {
		emit_qtype(param->type, true, out);
		xfprintf(out, " %%%s", param->name);
		if (param->next || def->func.variadic) {
			xfprintf(out, ", ");
		}
		param = param->next;
	}
	if (def->func.variadic) {
		xfprintf(out, "...");
	}
	xfprintf(out, ") {\n");

	for (size_t i = 0; i < def->func.prelude.ln; ++i) {
		struct qbe_statement *stmt = &def->func.prelude.stmts[i];
		emit_stmt(stmt, out);
	}

	for (size_t i = 0; i < def->func.body.ln; ++i) {
		struct qbe_statement *stmt = &def->func.body.stmts[i];
		emit_stmt(stmt, out);
	}

	xfprintf(out, "}\n\n");
}

static void
emit_data_string(const char *str, size_t sz, FILE *out)
{
	bool q = false;
	for (size_t i = 0; i < sz; ++i) {
		/* XXX: We could stand to emit less conservatively */
		if (!isprint((unsigned char)(str[i])) || str[i] == '"'
				|| str[i] == '\\') {
			if (q) {
				q = false;
				xfprintf(out, "\", ");
			}
			xfprintf(out, "b %d%s", str[i], i + 1 < sz ? ", " : "");
		} else {
			if (!q) {
				q = true;
				xfprintf(out, "b \"");
			}
			xfprintf(out, "%c", str[i]);
		}
	}
	if (q) {
		xfprintf(out, "\"");
	}
}

static bool
is_zeroes(struct qbe_data_item *data)
{
	for (struct qbe_data_item *cur = data; cur; cur = cur->next) {
		switch (cur->type) {
		case QD_ZEROED:
			break;
		case QD_VALUE:
			switch (cur->value.kind) {
			case QV_CONST:
				if (cur->value.lval != 0) {
					return false;
				}
				break;
			case QV_GLOBAL:
			case QV_LABEL:
			case QV_TEMPORARY:
				return false;
			case QV_VARIADIC:
				abort();
			}
			break;
		case QD_STRING:
			for (size_t i = 0; i < cur->sz; ++i) {
				if (cur->str[i] != 0) {
					return false;
				}
			}
			break;
		case QD_SYMOFFS:
			return false;
		}
	}
	return true;
}

static void
emit_data(struct qbe_def *def, FILE *out)
{
	assert(def->kind == Q_DATA);
	if (def->data.section && def->data.secflags) {
		xfprintf(out, "section \"%s\" \"%s\"",
				def->data.section, def->data.secflags);
	} else if (def->data.section) {
		xfprintf(out, "section \"%s\"", def->data.section);
	} else if (def->data.threadlocal) {
		if (is_zeroes(&def->data.items)) {
			xfprintf(out, "section \".tbss\" \"awT\"");
		} else {
			xfprintf(out, "section \".tdata\" \"awT\"");
		}
	} else if (is_zeroes(&def->data.items)) {
		xfprintf(out, "section \".bss.%s\"", def->name);
	} else {
		xfprintf(out, "section \".data.%s\"", def->name);
	}
	xfprintf(out, "%s\ndata $%s = ", def->exported ? " export" : "",
			def->name);
	if (def->data.align != ALIGN_UNDEFINED) {
		xfprintf(out, "align %zu ", def->data.align);
	}
	xfprintf(out, "{ ");

	struct qbe_data_item *item = &def->data.items;
	while (item) {
		switch (item->type) {
		case QD_VALUE:
			emit_qtype(item->value.type, true, out);
			xfprintf(out, " ");
			emit_value(&item->value, out);
			break;
		case QD_ZEROED:
			xfprintf(out, "z %zu", item->zeroed);
			break;
		case QD_STRING:
			emit_data_string(item->str, item->sz, out);
			break;
		case QD_SYMOFFS:
			// XXX: ARCH
			xfprintf(out, "l $%s + %ld", item->sym, item->offset);
			break;
		}

		xfprintf(out, item->next ? ", " : " ");
		item = item->next;
	}

	xfprintf(out, "}\n\n");
}

static void
emit_def(struct qbe_def *def, FILE *out)
{
	switch (def->kind) {
	case Q_TYPE:
		qemit_type(def, out);
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
