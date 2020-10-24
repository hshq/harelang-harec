#ifndef HAREC_UTF8_H
#define HAREC_UTF8_H

// Technically UTF-8 supports up to 6 byte codepoints, but Unicode itself
// doesn't really bother with more than 4.
#define UTF8_MAX_SIZE 4

#define UTF8_INVALID 0x80

/**
 * Grabs the next UTF-8 character and advances the string pointer
 */
uint32_t utf8_decode(const char **str);

/**
 * Encodes a character as UTF-8 and returns the length of that character.
 */
size_t utf8_encode(char *str, uint32_t ch);

/**
 * Returns the size of the next UTF-8 character
 */
int utf8_size(const char *str);

/**
 * Returns the size of a UTF-8 character
 */
size_t utf8_chsize(uint32_t ch);

/**
 * Reads and returns the next character from the file.
 */
uint32_t utf8_fgetch(FILE *f);

/**
 * Writes this character to the file and returns the number of bytes written.
 */
size_t utf8_fputch(FILE *f, uint32_t ch);

#endif
