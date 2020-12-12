#include "util.h"

unsigned long
djb2(unsigned long hash, char c)
{
	return ((hash << 5) + hash) + c;
}

unsigned long
djb2_s(unsigned long hash, const char *str)
{
	char c;
	while ((c = *str++)) {
		hash = djb2(hash, c);
	}
	return hash;
}
