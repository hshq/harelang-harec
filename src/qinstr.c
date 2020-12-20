#include "gen.h"
#include "qbe.h"

enum qbe_instr
alignment_to_qbe_alloc(size_t align)
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
