#include <stdlib.h>

void *c_memcpy(void *dest, const void *src, size_t n) {
	unsigned char *a = dest;
	const unsigned char *b = src;
	for (size_t i = 0; i < n; ++i) {
		a[i] = b[i];
	}
	return dest;
}
