#include <assert.h>
#include "expr.h"
#include "gen.h"
#include "qbe.h"

enum qbe_instr
alloc_for_align(size_t align)
{
	switch (align) {
	case 1:
	case 2:
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
	case Q__AGGREGATE:
		return Q_COPY;
	case Q__VOID:
		assert(0); // Invariant
	}
	assert(0);
}

enum qbe_instr
binarithm_for_op(enum binarithm_operator op,
	const struct qbe_type *type, bool is_signed)
{
	// TODO: NaN, udiv et al
	enum qbe_stype stype = type->stype;
	assert(stype != Q__AGGREGATE && stype != Q__VOID);
	switch (op) {
	case BIN_PLUS:
		return Q_ADD;
	case BIN_BAND:
		return Q_AND;
	case BIN_DIV:
		return is_signed ? Q_DIV : Q_UDIV;
	case BIN_MINUS:
		return Q_SUB;
	case BIN_TIMES:
		return Q_MUL;
	case BIN_MODULO:
		return is_signed ? Q_REM : Q_UREM;
	case BIN_BOR:
		return Q_OR;
	case BIN_BXOR:
		return Q_XOR;
	case BIN_LSHIFT:
		return Q_SHL;
	case BIN_RSHIFT:
		return Q_SHR;
	case BIN_LEQUAL:
		switch (stype) {
		case Q_WORD:
			return Q_CEQW;
		case Q_LONG:
			return Q_CEQL;
		case Q_SINGLE:
			return Q_CEQS;
		case Q_DOUBLE:
			return Q_CEQD;
		default:
			assert(0);
		}
		break;
	case BIN_NEQUAL:
	case BIN_LXOR:
		switch (stype) {
		case Q_WORD:
			return Q_CNEW;
		case Q_LONG:
			return Q_CNEL;
		case Q_SINGLE:
			return Q_CNES;
		case Q_DOUBLE:
			return Q_CNED;
		default:
			assert(0);
		}
		break;
	case BIN_GREATER:
		switch (stype) {
		case Q_WORD:
			return is_signed ? Q_CSGTW : Q_CUGTW;
		case Q_LONG:
			return is_signed ? Q_CSGTL : Q_CUGTL;
		case Q_SINGLE:
			return Q_CGTS;
		case Q_DOUBLE:
			return Q_CGTD;
		default:
			assert(0);
		}
	case BIN_GREATEREQ:
		switch (stype) {
		case Q_WORD:
			return is_signed ? Q_CSGEW : Q_CUGEW;
		case Q_LONG:
			return is_signed ? Q_CSGEL : Q_CUGEL;
		case Q_SINGLE:
			return Q_CGES;
		case Q_DOUBLE:
			return Q_CGED;
		default:
			assert(0);
		}
		break;
	case BIN_LESS:
		switch (stype) {
		case Q_WORD:
			return is_signed ? Q_CSLTW : Q_CULTW;
		case Q_LONG:
			return is_signed ? Q_CSLTL : Q_CULTL;
		case Q_SINGLE:
			return Q_CLTS;
		case Q_DOUBLE:
			return Q_CLTD;
		default:
			assert(0);
		}
		break;
	case BIN_LESSEQ:
		switch (stype) {
		case Q_WORD:
			return is_signed ? Q_CSLEW : Q_CULEW;
		case Q_LONG:
			return is_signed ? Q_CSLEL : Q_CULEL;
		case Q_SINGLE:
			return Q_CLES;
		case Q_DOUBLE:
			return Q_CLED;
		default:
			assert(0);
		}
		break;
	case BIN_LAND:
	case BIN_LOR:
		assert(0); // Handled elsewhere to address short circuiting
	}
	assert(0); // Unreachable
}
