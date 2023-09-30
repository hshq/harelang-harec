#ifndef HAREC_UTF8_H
#define HAREC_UTF8_H
#include <limits.h>
#include <stdio.h>

#define UTF8_MAX_SIZE 4

#define UTF8_INVALID UINT32_MAX

/**
 * Grabs the next UTF-8 codepoint and advances the string pointer
 */
uint32_t utf8_decode(const char **str);

/**
 * Encodes a codepoint as UTF-8 and returns the length of that codepoint.
 */
size_t utf8_encode(char *str, uint32_t ch);

/**
 * Reads and returns the next codepoint from the file.
 */
uint32_t utf8_get(FILE *f);

#endif
