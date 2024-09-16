#ifndef HARE_UTIL_H
#define HARE_UTIL_H
#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "lex.h"

enum exit_status {
	/* EXIT_SUCCESS = 0 (defined in stdlib.h) */
	EXIT_USER = 1,
	EXIT_LEX = 2,
	EXIT_PARSE = 3,
	EXIT_CHECK = 4,
	EXIT_ABNORMAL = 255,
};

extern const char **sources;
extern size_t nsources;

#define FNV1A_INIT 2166136261u

uint32_t fnv1a(uint32_t hash, unsigned char c);
uint32_t fnv1a_u32(uint32_t hash, uint32_t u32);
uint32_t fnv1a_u64(uint32_t hash, uint64_t u64);
uint32_t fnv1a_size(uint32_t hash, size_t sz);
uint32_t fnv1a_s(uint32_t hash, const char *str);
void *xcalloc(size_t n, size_t s);
void *xrealloc(void *p, size_t s);
char *xstrdup(const char *s);

#define FORMAT(FIRST)
#ifdef __has_attribute
#if __has_attribute(format)
#undef FORMAT
#define FORMAT(FIRST) __attribute__((format(printf, 2, FIRST)))
#endif
#endif

int xfprintf(FILE *restrict f, const char *restrict fmt, ...) FORMAT(3);
int xvfprintf(FILE *restrict f, const char *restrict fmt, va_list ap) FORMAT(0);

#undef FORMAT

#define malloc(a) (void *)sizeof(struct { static_assert(0, "Use xcalloc instead"); int _; })
#define calloc(a, b) (void *)sizeof(struct { static_assert(0, "Use xcalloc instead"); int _; })
#define realloc(a, b) (void *)sizeof(struct { static_assert(0, "Use xrealloc instead"); int _; })
#define strdup(s) (char *)(sizeof(struct { static_assert(0, "Use xstrdup instead"); int _; })

char *gen_name(int *id, const char *fmt);

void errline(struct location loc);

#endif
