#include <stdlib.h>
// Do not include this header:
//#include "util.h"

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

void *
xcalloc(size_t n, size_t s)
{
	void *p = calloc(n, s);
	if (!p) {
		abort();
	}
	return p;
}

void *
xrealloc(void *p, size_t s)
{
	p = realloc(p, s);
	if (!p) {
		abort();
	}
	return p;
}
