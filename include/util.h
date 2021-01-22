#ifndef HARE_UTIL_H
#define HARE_UTIL_H
#include <assert.h>
#include <stdint.h>

#define FNV1A_INIT 2166136261u

uint32_t fnv1a(uint32_t hash, unsigned char c);
uint32_t fnv1a_u32(uint32_t hash, uint32_t c);
uint32_t fnv1a_s(uint32_t hash, const char *str);
void *xcalloc(size_t n, size_t s);
void *xrealloc(void *p, size_t s);

#define malloc(a) (void *)sizeof(struct { static_assert(0, "Use xcalloc instead"); int _; });
#define calloc(a, b) (void *)sizeof(struct { static_assert(0, "Use xcalloc instead"); int _; });
#define realloc(a, b) (void *)sizeof(struct { static_assert(0, "Use xrealloc instead"); int _; });

struct pathspec {
	const char *var;
	const char *path;
};

char *getpath(const struct pathspec *paths, size_t npaths);

#endif
