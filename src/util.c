#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "util.h"
// Remove safety macros:
#undef malloc
#undef calloc
#undef realloc

const char **sources;

uint32_t
fnv1a(uint32_t hash, unsigned char c)
{
	return (hash ^ c) * 16777619;
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
fnv1a_u64(uint32_t hash, uint64_t u64)
{
	hash = fnv1a(hash, (u64) & 0xFF);
	hash = fnv1a(hash, (u64 >> 8) & 0xFF);
	hash = fnv1a(hash, (u64 >> 16) & 0xFF);
	hash = fnv1a(hash, (u64 >> 24) & 0xFF);
	hash = fnv1a(hash, (u64 >> 32) & 0xFF);
	hash = fnv1a(hash, (u64 >> 40) & 0xFF);
	hash = fnv1a(hash, (u64 >> 48) & 0xFF);
	hash = fnv1a(hash, (u64 >> 54) & 0xFF);
	return hash;
}

uint32_t
fnv1a_size(uint32_t hash, size_t sz)
{
	for (size_t i = 0; i < sizeof(sz); i++) {
		hash = fnv1a(hash, sz & 0xFF);
		sz >>= 8;
	}
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
	if (!p && s) {
		abort();
	}
	return p;
}

void *
xrealloc(void *p, size_t s)
{
	p = realloc(p, s);
	if (!p && s) {
		abort();
	}
	return p;
}

char *
getpath(const struct pathspec *paths, size_t npaths) {
	for (size_t i = 0; i < npaths; i++) {
		const char *var = "";
		if (paths[i].var) {
			var = getenv(paths[i].var);
		}
		if (var) {
			char *out = calloc(1,
				strlen(var) + strlen(paths[i].path) + 1);
			strcat(strcat(out, var), paths[i].path);
			return out;
		}
	}
	return NULL;
}
