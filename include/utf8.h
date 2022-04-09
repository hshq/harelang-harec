#ifndef HAREC_UTF8_H
#define HAREC_UTF8_H

#define UTF8_MAX_SIZE 4

#define UTF8_INVALID 0x80

/**
 * Grabs the next UTF-8 codepoint and advances the string pointer
 */
uint32_t utf8_decode(const char **str);

/**
 * Encodes a codepoint as UTF-8 and returns the length of that codepoint.
 */
size_t utf8_encode(char *str, uint32_t ch);

/**
 * Returns the size of the next UTF-8 codepoint
 */
int utf8_size(const char *str);

/**
 * Returns the size of a UTF-8 codepoint
 */
size_t utf8_cpsize(uint32_t ch);

/**
 * Reads and returns the next codepoint from the file.
 */
uint32_t utf8_get(FILE *f);

#endif
