#include <stdlib.h>
#include <unistd.h>

void *c_memcpy(void *dest, const void *src, size_t n) {
	unsigned char *a = dest;
	const unsigned char *b = src;
	for (size_t i = 0; i < n; ++i) {
		a[i] = b[i];
	}
	return dest;
}

struct ha_str {
	size_t len, cap;
	char *str;
};

void c_abort(struct ha_str str) {
	write(2, str.str, str.len);
	write(2, "\n", 1);
	abort();
}
