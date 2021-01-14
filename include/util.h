#ifndef HARE_UTIL_H
#define HARE_UTIL_H
#include <assert.h>
#include <stdint.h>

#define FNV1A_INIT 14695981039346656037UL

uint64_t fnv1a(uint64_t hash, unsigned char c);
uint64_t fnv1a_u64(uint64_t hash, uint64_t c);
uint64_t fnv1a_s(uint64_t hash, const char *str);
void *xcalloc(size_t n, size_t s);
void *xrealloc(void *p, size_t s);

#define malloc(a) (void *)sizeof(struct { static_assert(0, "Use xcalloc instead"); int _; });
#define calloc(a, b) (void *)sizeof(struct { static_assert(0, "Use xcalloc instead"); int _; });
#define realloc(a, b) (void *)sizeof(struct { static_assert(0, "Use xrealloc instead"); int _; });

#endif
