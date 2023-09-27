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
},
qbe_half = {
	.stype = Q_HALF,
	.size = 2,
},
qbe_word = {
	.stype = Q_WORD,
	.size = 4,
},
qbe_long = {
	.stype = Q_LONG,
	.size = 8,
},
qbe_single = {
	.stype = Q_SINGLE,
	.size = 4,
},
qbe_double = {
	.stype = Q_DOUBLE,
	.size = 8,
},
qbe_void = {
	.stype = Q__VOID,
},
qbe_aggregate = {
	.stype = Q__AGGREGATE,
};

const struct qbe_value variadic_sigil = {0};

const char *qbe_instr[Q_LAST_INSTR] = {
	[Q_ADD] = "add",
	[Q_ALLOC16] = "alloc16",
	[Q_ALLOC4] = "alloc4",
	[Q_ALLOC8] = "alloc8",
	[Q_AND] = "and",
	[Q_BLIT] = "blit",
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
	[Q_DBGLOC] = "dbgloc",
	[Q_DIV] = "div",
	[Q_DTOSI] = "dtosi",
	[Q_DTOUI] = "dtoui",
	[Q_EXTS] = "exts",
	[Q_EXTSB] = "extsb",
	[Q_EXTSH] = "extsh",
	[Q_EXTSW] = "extsw",
	[Q_EXTUB] = "extub",
	[Q_EXTUH] = "extuh",
	[Q_EXTUW] = "extuw",
	[Q_HLT] = "hlt",
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
	[Q_NEG] = "neg",
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
	[Q_STOUI] = "stoui",
	[Q_SUB] = "sub",
	[Q_SWTOF] = "swtof",
	[Q_TRUNCD] = "truncd",
	[Q_UDIV] = "udiv",
	[Q_ULTOF] = "ultof",
	[Q_UREM] = "urem",
	[Q_UWTOF] = "uwtof",
	[Q_VAARG] = "vaarg",
	[Q_VASTART] = "vastart",
	[Q_XOR] = "xor",
};

void
qbe_append_def(struct qbe_program *prog, struct qbe_def *def)
{
	*prog->next = def;
	prog->next = &def->next;
}

static struct qbe_value *
qval_dup(const struct qbe_value *val)
{
	struct qbe_value *new = xcalloc(1, sizeof(struct qbe_value));
	*new = *val;
	if (val->kind != QV_CONST) {
		new->name = xstrdup(val->name);
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
		assert(out->kind == QV_TEMPORARY);
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
push(struct qbe_statements *stmts, struct qbe_statement *stmt)
{
	if (!stmts->stmts) {
		stmts->sz = 256;
		stmts->ln = 0;
		stmts->stmts = xcalloc(stmts->sz,
			sizeof(struct qbe_statement));
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

	struct qbe_value hack;
	if (out && (out->type->stype == Q_BYTE || out->type->stype == Q_HALF)) {
		hack = *out;
		hack.type = &qbe_word;
		out = &hack;
	}

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

struct qbe_value
constl(uint64_t l)
{
	return (struct qbe_value){
		.kind = QV_CONST,
		.type = &qbe_long,
		.lval = l,
	};
}

struct qbe_value
constw(uint32_t w)
{
	return (struct qbe_value){
		.kind = QV_CONST,
		.type = &qbe_word,
		.wval = w,
	};
}

struct qbe_value
consts(float s)
{
	return (struct qbe_value){
		.kind = QV_CONST,
		.type = &qbe_single,
		.sval = s,
	};
}

struct qbe_value
constd(double d)
{
	return (struct qbe_value){
		.kind = QV_CONST,
		.type = &qbe_double,
		.dval = d,
	};
}
