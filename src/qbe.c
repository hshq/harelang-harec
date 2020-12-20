#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include "qbe.h"

// Simple type singletons
const struct qbe_type
qbe_byte = {
	.stype = Q_BYTE,
},
qbe_half = {
	.stype = Q_HALF,
},
qbe_word = {
	.stype = Q_WORD,
},
qbe_long = {
	.stype = Q_LONG,
},
qbe_single = {
	.stype = Q_SINGLE,
},
qbe_double = {
	.stype = Q_DOUBLE,
},
qbe_void = {
	.stype = Q__VOID,
};

const struct qbe_type *
qtype_for_xtype(enum qbe_stype type)
{
	switch (type) {
	case Q_BYTE:
		return &qbe_byte;
	case Q_HALF:
		return &qbe_half;
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
		assert(0); // Invariant
	}
	assert(0); // Unreachable
}

const char *qbe_instr[Q_LAST_INSTR] = {
	[Q_ADD] = "add",
	[Q_ALLOC16] = "alloc16",
	[Q_ALLOC4] = "alloc4",
	[Q_ALLOC8] = "alloc8",
	[Q_AND] = "and",
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
	def->next = prog->defs;
	prog->defs = def;
}

void
geni(struct qbe_statement *stmt, enum qbe_instr instr, ...)
{
	assert(0); // TODO
}

void
genl(struct qbe_statement *stmt, uint64_t *id, const char *fmt, ...)
{
	assert(0); // TODO
}

void
pushi(struct qbe_func *func, enum qbe_instr instr, ...)
{
	assert(0); // TODO
}

void
pushl(struct qbe_func *func, uint64_t *id, const char *fmt, ...)
{
	assert(0); // TODO
}
