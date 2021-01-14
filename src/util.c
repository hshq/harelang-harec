#include <stdlib.h>
#include <stdint.h>
// Do not include this header:
//#include "util.h"

uint64_t
fnv1a(uint64_t hash, unsigned char c)
{
	return (hash ^ c) * 1099511628211;
}

uint64_t
fnv1a_s(uint64_t hash, const char *str)
{
	unsigned char c;
	while ((c = *str++)) {
		hash = fnv1a(hash, c);
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
