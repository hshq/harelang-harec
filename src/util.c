#include <stdlib.h>
#include <stdint.h>
// Do not include this header:
//#include "util.h"

uint32_t
fnv1a(uint32_t hash, unsigned char c)
{
	return (hash ^ c) * 1099511628211;
}

uint32_t
fnv1a_u32(uint32_t hash, uint32_t u32)
{
	hash = fnv1a(hash, (u32) & 0xFF);
	hash = fnv1a(hash, (u32 >> 8) & 0xFF);
	hash = fnv1a(hash, (u32 >> 16) & 0xFF);
	hash = fnv1a(hash, (u32 >> 24) & 0xFF);
	return hash;
}

uint32_t
fnv1a_s(uint32_t hash, const char *str)
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
