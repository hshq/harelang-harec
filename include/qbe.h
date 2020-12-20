#ifndef HAREC_QBE_H
#define HAREC_QBE_H
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>

enum qbe_stype {
	Q__VOID = 0,
	Q_BYTE = 'b',
	Q_HALF = 'h',
	Q_WORD = 'w',
	Q_LONG = 'l',
	Q_SINGLE = 's',
	Q_DOUBLE = 'd',
	Q__AGGREGATE,
};

struct qbe_type {
	enum qbe_stype stype;
	// TODO: Aggregate type details
};

// Simple type singletons
extern const struct qbe_type
	qbe_byte,
	qbe_half,
	qbe_word,
	qbe_long,
	qbe_single,
	qbe_double,
	qbe_void;

const struct qbe_type *qtype_for_xtype(enum qbe_stype type);

enum qbe_value_kind {
	QV_CONST,
	QV_GLOBAL,
	QV_LABEL,
	QV_TEMPORARY,
};

struct qbe_value {
	enum qbe_value_kind kind;
	const struct qbe_type *type;
	union {
		char *name;
		uint32_t wval;
		uint64_t lval;
		float sval;
		double dval;
	};
};

enum qbe_instr {
	Q_ADD,
	Q_ALLOC16,
	Q_ALLOC4,
	Q_ALLOC8,
	Q_AND,
	Q_CEQD,
	Q_CEQL,
	Q_CEQS,
	Q_CEQW,
	Q_CGED,
	Q_CGES,
	Q_CGTD,
	Q_CGTS,
	Q_CLED,
	Q_CLES,
	Q_CLTD,
	Q_CLTS,
	Q_CNED,
	Q_CNEL,
	Q_CNES,
	Q_CNEW,
	Q_COD,
	Q_COS,
	Q_CSGEL,
	Q_CSGEW,
	Q_CSGTL,
	Q_CSGTW,
	Q_CSLEL,
	Q_CSLEW,
	Q_CSLTL,
	Q_CSLTW,
	Q_CUGEL,
	Q_CUGEW,
	Q_CUGTL,
	Q_CUGTW,
	Q_CULEL,
	Q_CULEW,
	Q_CULTL,
	Q_CULTW,
	Q_CUOD,
	Q_CUOS,
	Q_DIV,
	Q_DTOSI,
	Q_EXTS,
	Q_EXTSB,
	Q_EXTSH,
	Q_EXTSW,
	Q_EXTUB,
	Q_EXTUH,
	Q_EXTUW,
	Q_JMP,
	Q_JNZ,
	Q_LOADD,
	Q_LOADL,
	Q_LOADS,
	Q_LOADSB,
	Q_LOADSH,
	Q_LOADSW,
	Q_LOADUB,
	Q_LOADUH,
	Q_LOADUW,
	Q_MUL,
	Q_OR,
	Q_REM,
	Q_RET,
	Q_SAR,
	Q_SHL,
	Q_SHR,
	Q_SLTOF,
	Q_STOREB,
	Q_STORED,
	Q_STOREH,
	Q_STOREL,
	Q_STORES,
	Q_STOREW,
	Q_STOSI,
	Q_SUB,
	Q_SWTOF,
	Q_TRUNCD,
	Q_UDIV,
	Q_UREM,
	Q_XOR,

	Q_LAST_INSTR,
};

extern const char *qbe_instr[Q_LAST_INSTR];

enum qbe_statement_type {
	Q_COMMENT,
	Q_INSTR,
	Q_LABEL,
};

struct qbe_arguments {
	struct qbe_value value;
	struct qbe_arguments *next;
};

struct qbe_statement {
	enum qbe_statement_type type;
	union {
		struct {
			enum qbe_instr instr;
			struct qbe_value *out;
			struct qbe_arguments *args;
		};
		char *label;
		char *comment;
	};
};

struct qbe_func {
	const struct qbe_type *returns;
	// TODO: Parameters
	size_t blen, bsiz;
	struct qbe_statement *body;
};

enum qbe_deftype {
	Q_TYPE,
	Q_FUNC,
	Q_DATA,
};

struct qbe_def {
	char *name;
	enum qbe_deftype type;
	bool exported;
	union {
		struct qbe_func func;
	};
	struct qbe_def *next;
};

struct qbe_program {
	struct qbe_def *defs;
};

void qbe_append_def(struct qbe_program *prog, struct qbe_def *def);

void geni(struct qbe_statement *stmt, enum qbe_instr instr, const struct qbe_value *out, ...);
const char *genl(struct qbe_statement *stmt, uint64_t *id, const char *fmt);
void pushi(struct qbe_func *func, enum qbe_instr instr, const struct qbe_value *out, ...);
const char *pushl(struct qbe_func *func, uint64_t *id, const char *fmt);
void pushc(struct qbe_func *func, const char *text);

void constw(struct qbe_value *val, uint32_t l);
void constl(struct qbe_value *val, uint64_t l);
void consts(struct qbe_value *val, float l);
void constd(struct qbe_value *val, double l);

#endif
