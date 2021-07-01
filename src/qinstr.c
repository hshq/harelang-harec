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
