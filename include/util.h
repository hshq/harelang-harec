#ifndef HARE_UTIL_H
#define HARE_UTIL_H
#include <assert.h>

#define DJB2_INIT 5381

unsigned long djb2(unsigned long hash, char c);
unsigned long djb2_s(unsigned long hash, const char *str);
void *xcalloc(size_t n, size_t s);
void *xrealloc(void *p, size_t s);

#define calloc(a, b) (void *)sizeof(struct { static_assert(0, "Use xcalloc instead"); int _; });
#define realloc(a, b) (void *)sizeof(struct { static_assert(0, "Use xrealloc instead"); int _; });

#endif
