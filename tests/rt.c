#include <stdlib.h>
#include <string.h>
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
	char *str;
	size_t len, cap;
};

void c_abort(struct ha_str str) {
	write(2, str.str, str.len);
	write(2, "\n", 1);
	abort();
}

const char *reasons[] = {
	"Slice or array access out of bounds",	// 0
	"Type assertion failed",		// 1
	"Out of memory",			// 2
};

void c_abort_fixed(struct ha_str loc, int i) {
	write(2, "Abort: ", 7);
	write(2, loc.str, loc.len);
	write(2, ": ", 2);
	write(2, reasons[i], strlen(reasons[i]));
	write(2, "\n", 1);
	abort();
};
