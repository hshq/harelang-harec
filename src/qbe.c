#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "qbe.h"
#include "util.h"

// Simple type singletons
const struct qbe_type
qbe_byte = {
	.stype = Q_BYTE,
	.size = 1,
	.align = 1,
},
qbe_byte_s = {
	.stype = Q_BYTE,
	.size = 1,
	.align = 1,
	.is_signed = true,
},
qbe_half = {
	.stype = Q_HALF,
	.size = 2,
	.align = 2,
},
qbe_half_s = {
	.stype = Q_HALF,
	.size = 2,
	.align = 2,
	.is_signed = true,
},
qbe_word = {
	.stype = Q_WORD,
	.size = 4,
	.align = 4,
},
qbe_long = {
	.stype = Q_LONG,
	.size = 8,
	.align = 8,
},
qbe_single = {
	.stype = Q_SINGLE,
	.size = 4,
	.align = 4,
},
qbe_double = {
	.stype = Q_DOUBLE,
	.size = 8,
	.align = 8,
},
qbe_void = {
	.stype = Q__VOID,
},
// Used for some types which are unrepresentible in the qbe type system, but
// still representable as values (e.g. functions)
qbe_aggregate = {
	.stype = Q__AGGREGATE,
};

const struct qbe_type *
qtype_for_xtype(enum qbe_stype type, bool is_signed)
{
	switch (type) {
	case Q_BYTE:
		return is_signed ? &qbe_byte_s : &qbe_byte;
	case Q_HALF:
		return is_signed ? &qbe_half_s : &qbe_half;
	case Q_WORD:
		return &qbe_word;
	case Q_LONG:
		return &qbe_long;
	case Q_SINGLE:
		return &qbe_single;
	case Q_DOUBLE:
		return &qbe_double;
	case Q__VOID:
		return &qbe_void;
	case Q__AGGREGATE:
		return &qbe_aggregate;
	}
	assert(0); // Unreachable
}

const char *qbe_instr[Q_LAST_INSTR] = {
	[Q_ADD] = "add",
	[Q_ALLOC16] = "alloc16",
	[Q_ALLOC4] = "alloc4",
	[Q_ALLOC8] = "alloc8",
	[Q_AND] = "and",
	[Q_CALL] = "call",
	[Q_CAST] = "cast",
	[Q_CEQD] = "ceqd",
	[Q_CEQL] = "ceql",
	[Q_CEQS] = "ceqs",
	[Q_CEQW] = "ceqw",
	[Q_CGED] = "cged",
	[Q_CGES] = "cges",
	[Q_CGTD] = "cgtd",
	[Q_CGTS] = "cgts",
	[Q_CLED] = "cled",
	[Q_CLES] = "cles",
	[Q_CLTD] = "cltd",
	[Q_CLTS] = "clts",
	[Q_CNED] = "cned",
	[Q_CNEL] = "cnel",
	[Q_CNES] = "cnes",
	[Q_CNEW] = "cnew",
	[Q_COD] = "cod",
	[Q_COPY] = "copy",
	[Q_COS] = "cos",
	[Q_CSGEL] = "csgel",
	[Q_CSGEW] = "csgew",
	[Q_CSGTL] = "csgtl",
	[Q_CSGTW] = "csgtw",
	[Q_CSLEL] = "cslel",
	[Q_CSLEW] = "cslew",
	[Q_CSLTL] = "csltl",
	[Q_CSLTW] = "csltw",
	[Q_CUGEL] = "cugel",
	[Q_CUGEW] = "cugew",
	[Q_CUGTL] = "cugtl",
	[Q_CUGTW] = "cugtw",
	[Q_CULEL] = "culel",
	[Q_CULEW] = "culew",
	[Q_CULTL] = "cultl",
	[Q_CULTW] = "cultw",
	[Q_CUOD] = "cuod",
	[Q_CUOS] = "cuos",
	[Q_DIV] = "div",
	[Q_DTOSI] = "dtosi",
	[Q_EXTS] = "exts",
	[Q_EXTSB] = "extsb",
	[Q_EXTSH] = "extsh",
	[Q_EXTSW] = "extsw",
	[Q_EXTUB] = "extub",
	[Q_EXTUH] = "extuh",
	[Q_EXTUW] = "extuw",
	[Q_JMP] = "jmp",
	[Q_JNZ] = "jnz",
	[Q_LOADD] = "loadd",
	[Q_LOADL] = "loadl",
	[Q_LOADS] = "loads",
	[Q_LOADSB] = "loadsb",
	[Q_LOADSH] = "loadsh",
	[Q_LOADSW] = "loadsw",
	[Q_LOADUB] = "loadub",
	[Q_LOADUH] = "loaduh",
	[Q_LOADUW] = "loaduw",
	[Q_MUL] = "mul",
	[Q_OR] = "or",
	[Q_REM] = "rem",
	[Q_RET] = "ret",
	[Q_SAR] = "sar",
	[Q_SHL] = "shl",
	[Q_SHR] = "shr",
	[Q_SLTOF] = "sltof",
	[Q_STOREB] = "storeb",
	[Q_STORED] = "stored",
	[Q_STOREH] = "storeh",
	[Q_STOREL] = "storel",
	[Q_STORES] = "stores",
	[Q_STOREW] = "storew",
	[Q_STOSI] = "stosi",
	[Q_SUB] = "sub",
	[Q_SWTOF] = "swtof",
	[Q_TRUNCD] = "truncd",
	[Q_UDIV] = "udiv",
	[Q_UREM] = "urem",
	[Q_XOR] = "xor",
};

void
qbe_append_def(struct qbe_program *prog, struct qbe_def *def)
{
	*prog->next = def;
	prog->next = &def->next;
}

struct qbe_value *
qval_dup(const struct qbe_value *val)
{
	struct qbe_value *new = xcalloc(1, sizeof(struct qbe_value));
	*new = *val;
	if (val->kind != QV_CONST) {
		new->name = strdup(val->name);
	}
	return new;
}

static void
va_geni(struct qbe_statement *stmt, enum qbe_instr instr,
		const struct qbe_value *out, va_list ap)
{
	stmt->type = Q_INSTR;
	stmt->instr = instr;

	if (out) {
		stmt->out = qval_dup(out);
	}

	struct qbe_arguments **next = &stmt->args;
	struct qbe_value *val;
	while ((val = va_arg(ap, struct qbe_value *))) {
		struct qbe_arguments *arg = xcalloc(1, sizeof(struct qbe_arguments));
		arg->value = *val;
		*next = arg;
		next = &arg->next;
	}
}

void
geni(struct qbe_statement *stmt, const struct qbe_value *out,
		enum qbe_instr instr, ...)
{
	va_list ap;
	va_start(ap, instr);
	va_geni(stmt, instr, out, ap);
	va_end(ap);
}

const char *
genl(struct qbe_statement *stmt, uint64_t *id, const char *fmt)
{
	stmt->type = Q_LABEL;
	int n = snprintf(NULL, 0, fmt, *id);
	char *l = xcalloc(1, n + 1);
	snprintf(l, n + 1, fmt, *id);
	stmt->label = l;
	*id = *id + 1;
	return l;
}

void
push(struct qbe_statements *stmts, struct qbe_statement *stmt)
{
	if (!stmts->stmts) {
		stmts->sz = 256;
		stmts->ln = 0;
		stmts->stmts = xcalloc(1,
			sizeof(struct qbe_statement) * stmts->sz);
	}
	if (stmts->ln + 1 >= stmts->sz) {
		stmts->sz *= 2;
		stmts->stmts = xrealloc(stmts->stmts,
			sizeof(struct qbe_statement) * stmts->sz);
	}
	stmts->stmts[stmts->ln++] = *stmt;
}

void
pushi(struct qbe_func *func, const struct qbe_value *out,
		enum qbe_instr instr, ...)
{
	struct qbe_statement stmt = {0};
	va_list ap;
	va_start(ap, instr);
	va_geni(&stmt, instr, out, ap);
	va_end(ap);
	push(&func->body, &stmt);
}

void
pushprei(struct qbe_func *func, const struct qbe_value *out,
		enum qbe_instr instr, ...)
{
	struct qbe_statement stmt = {0};
	va_list ap;
	va_start(ap, instr);
	va_geni(&stmt, instr, out, ap);
	va_end(ap);
	push(&func->prelude, &stmt);
}

const char *
pushl(struct qbe_func *func, uint64_t *id, const char *fmt)
{
	struct qbe_statement stmt = {0};
	const char *l = genl(&stmt, id, fmt);
	push(&func->body, &stmt);
	return l;
}

void
pushc(struct qbe_func *func, const char *fmt, ...)
{
	struct qbe_statement stmt = {0};

	va_list ap;
	va_start(ap, fmt);
	int n = vsnprintf(NULL, 0, fmt, ap);
	va_end(ap);

	char *str = xcalloc(1, n + 1);
	va_start(ap, fmt);
	vsnprintf(str, n + 1, fmt, ap);
	va_end(ap);

	stmt.comment = str;
	push(&func->body, &stmt);
}

void
constw(struct qbe_value *val, uint32_t w)
{
	val->kind = QV_CONST;
	val->type = &qbe_word;
	val->wval = w;
	val->indirect = false;
}

void
constl(struct qbe_value *val, uint64_t l)
{
	val->kind = QV_CONST;
	val->type = &qbe_long;
	val->lval = l;
	val->indirect = false;
}

void
consts(struct qbe_value *val, float s)
{
	val->kind = QV_CONST;
	val->type = &qbe_single;
	val->sval = s;
	val->indirect = false;
}

void
constd(struct qbe_value *val, double d)
{
	val->kind = QV_CONST;
	val->type = &qbe_double;
	val->dval = d;
	val->indirect = false;
}

void
const_void(struct qbe_value *val)
{
	val->kind = QV_CONST;
	val->type = &qbe_void;
	val->indirect = false;
}
