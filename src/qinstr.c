#include <assert.h>
#include "expr.h"
#include "gen.h"
#include "qbe.h"

enum qbe_instr
alloc_for_align(size_t align)
{
	switch (align) {
	case 4:
		return Q_ALLOC4;
	case 8:
		return Q_ALLOC8;
	default:
		return Q_ALLOC16;
	}
}

enum qbe_instr
store_for_type(enum qbe_stype stype)
{
	switch (stype) {
	case Q_BYTE:
		return Q_STOREB;
	case Q_HALF:
		return Q_STOREH;
	case Q_WORD:
		return Q_STOREW;
	case Q_LONG:
		return Q_STOREL;
	case Q_SINGLE:
		return Q_STORES;
	case Q_DOUBLE:
		return Q_STORED;
	case Q__VOID:
	case Q__AGGREGATE:
		assert(0); // Invariant
	}
	assert(0);
}

enum qbe_instr
load_for_type(enum qbe_stype stype, bool is_signed)
{
	switch (stype) {
	case Q_BYTE:
		return is_signed ? Q_LOADSB : Q_LOADUB;
	case Q_HALF:
		return is_signed ? Q_LOADSH : Q_LOADUH;
	case Q_WORD:
		return is_signed ? Q_LOADSW : Q_LOADUW;
	case Q_LONG:
		return Q_LOADL;
	case Q_SINGLE:
		return Q_LOADS;
	case Q_DOUBLE:
		return Q_LOADD;
	case Q__VOID:
	case Q__AGGREGATE:
		assert(0); // Invariant
	}
	assert(0);
}

enum qbe_instr
binarithm_for_op(enum binarithm_operator op)
{
	// TODO: udiv et al
	switch (op) {
	case BIN_PLUS:
		return Q_ADD;
	case BIN_BAND:
		return Q_AND;
	case BIN_DIV:
		return Q_DIV;
	case BIN_MINUS:
		return Q_SUB;
	case BIN_TIMES:
		return Q_MUL;
	case BIN_MODULO:
		return Q_REM;
	case BIN_BOR:
		return Q_OR;
	case BIN_BXOR:
		return Q_XOR;
	case BIN_LSHIFT:
		return Q_SHL;
	case BIN_RSHIFT:
		return Q_SHR;
	case BIN_BNOT:
	case BIN_GREATER:
	case BIN_GREATEREQ:
	case BIN_LAND:
	case BIN_LEQUAL:
	case BIN_LESS:
	case BIN_LESSEQ:
	case BIN_LOR:
	case BIN_LXOR:
	case BIN_NEQUAL:
		assert(0); // Invariant
	}
	assert(0); // Unreachable
}
