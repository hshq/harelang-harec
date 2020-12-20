#include <assert.h>
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
