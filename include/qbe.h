#ifndef HAREC_QBE_H
#define HAREC_QBE_H
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

enum qbe_stype {
	Q__VOID = 'V',
	Q_BYTE = 'b',
	Q_HALF = 'h',
	Q_WORD = 'w',
	Q_LONG = 'l',
	Q_SINGLE = 's',
	Q_DOUBLE = 'd',
	Q__AGGREGATE = 'A',
};

struct qbe_type;

struct qbe_field {
	const struct qbe_type *type;
	size_t count;
	struct qbe_field *next;
};

struct qbe_type {
	enum qbe_stype stype;
	size_t size;

	// Aggregate types only:
	char *name;
	struct qbe_field fields;
	const struct type *base;
};

// Simple type singletons
extern const struct qbe_type
	qbe_byte,
	qbe_half,
	qbe_word,
	qbe_long,
	qbe_single,
	qbe_double,
	qbe_void,
	qbe_aggregate;

enum qbe_value_kind {
	QV_CONST,
	QV_GLOBAL,
	QV_LABEL,
	QV_TEMPORARY,
	QV_VARIADIC,
};

struct qbe_value {
	enum qbe_value_kind kind;
	bool threadlocal;
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
	Q_BLIT,
	Q_CALL,
	Q_CAST,
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
	Q_COPY,
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
	Q_DBGLOC,
	Q_DIV,
	Q_DTOSI,
	Q_DTOUI,
	Q_EXTS,
	Q_EXTSB,
	Q_EXTSH,
	Q_EXTSW,
	Q_EXTUB,
	Q_EXTUH,
	Q_EXTUW,
	Q_HLT,
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
	Q_NEG,
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
	Q_STOUI,
	Q_SUB,
	Q_SWTOF,
	Q_TRUNCD,
	Q_UDIV,
	Q_ULTOF,
	Q_UREM,
	Q_UWTOF,
	Q_VAARG,
	Q_VASTART,
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

struct qbe_func_param {
	char *name;
	const struct qbe_type *type;
	struct qbe_func_param *next;
};

struct qbe_statements {
	size_t ln, sz;
	struct qbe_statement *stmts;
};

struct qbe_func {
	const struct qbe_type *returns;
	struct qbe_func_param *params;
	bool variadic;
	struct qbe_statements prelude, body;
};

enum qbe_datatype {
	QD_VALUE,
	QD_ZEROED,
	QD_STRING,
	QD_SYMOFFS,
};

struct qbe_data_item {
	enum qbe_datatype type;
	union {
		struct qbe_value value;
		size_t zeroed;
		struct {
			char *str;
			size_t sz;
		};
		struct {
			char *sym;
			int64_t offset;
		};
	};
	struct qbe_data_item *next;
};

struct qbe_data {
	size_t align;
	char *section, *secflags;
	bool threadlocal;
	struct qbe_data_item items;
};

enum qbe_defkind {
	Q_TYPE,
	Q_FUNC,
	Q_DATA,
};

struct qbe_def {
	char *name;
	int file;
	enum qbe_defkind kind;
	bool exported;
	union {
		struct qbe_func func;
		struct qbe_type type;
		struct qbe_data data;
	};
	struct qbe_def *next;
};

struct qbe_program {
	struct qbe_def *defs;
	struct qbe_def **next;
};

void qbe_append_def(struct qbe_program *prog, struct qbe_def *def);

void pushi(struct qbe_func *func, const struct qbe_value *out, enum qbe_instr instr, ...);
void pushprei(struct qbe_func *func, const struct qbe_value *out, enum qbe_instr instr, ...);
void pushc(struct qbe_func *func, const char *fmt, ...);
void push(struct qbe_statements *stmts, struct qbe_statement *stmt);

struct qbe_value constl(uint64_t l);
struct qbe_value constw(uint32_t w);
struct qbe_value consts(float s);
struct qbe_value constd(double d);

#endif
