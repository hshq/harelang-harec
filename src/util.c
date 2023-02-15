#include <sys/stat.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include "util.h"
// Remove safety macros:
#undef malloc
#undef calloc
#undef realloc
#undef strdup

const char **sources;

uint32_t
fnv1a(uint32_t hash, unsigned char c)
{
	return (hash ^ c) * 16777619;
}

uint32_t
fnv1a_u32(uint32_t hash, uint32_t u32)
{
	hash = fnv1a(hash, (u32) & 0xFF);
	hash = fnv1a(hash, (u32 >> 8) & 0xFF);
	hash = fnv1a(hash, (u32 >> 16) & 0xFF);
	hash = fnv1a(hash, (u32 >> 24) & 0xFF);
	return hash;
}

uint32_t
fnv1a_u64(uint32_t hash, uint64_t u64)
{
	hash = fnv1a(hash, (u64) & 0xFF);
	hash = fnv1a(hash, (u64 >> 8) & 0xFF);
	hash = fnv1a(hash, (u64 >> 16) & 0xFF);
	hash = fnv1a(hash, (u64 >> 24) & 0xFF);
	hash = fnv1a(hash, (u64 >> 32) & 0xFF);
	hash = fnv1a(hash, (u64 >> 40) & 0xFF);
	hash = fnv1a(hash, (u64 >> 48) & 0xFF);
	hash = fnv1a(hash, (u64 >> 54) & 0xFF);
	return hash;
}

uint32_t
fnv1a_size(uint32_t hash, size_t sz)
{
	for (size_t i = 0; i < sizeof(sz); i++) {
		hash = fnv1a(hash, sz & 0xFF);
		sz >>= 8;
	}
	return hash;
}

uint32_t
fnv1a_s(uint32_t hash, const char *str)
{
	unsigned char c;
	while ((c = *str++)) {
		hash = fnv1a(hash, c);
	}
	return hash;
}

void *
xcalloc(size_t n, size_t s)
{
	void *p = calloc(n, s);
	if (!p && s) {
		abort();
	}
	return p;
}

void *
xrealloc(void *p, size_t s)
{
	p = realloc(p, s);
	if (!p && s) {
		abort();
	}
	return p;
}

char *
xstrdup(const char *s)
{
	char *ret = strdup(s);
	if (!ret) {
		abort();
	}
	return ret;
}

char *
gen_name(int *id, const char *fmt)
{
	int n = snprintf(NULL, 0, fmt, *id);
	char *str = xcalloc(1, n + 1);
	snprintf(str, n + 1, fmt, *id);
	++*id;
	return str;
}

char *
getpath(const struct pathspec *paths, size_t npaths) {
	for (size_t i = 0; i < npaths; i++) {
		const char *var = "";
		if (paths[i].var) {
			var = getenv(paths[i].var);
		}
		if (var) {
			char *out = calloc(1,
				strlen(var) + strlen(paths[i].path) + 1);
			strcat(strcat(out, var), paths[i].path);
			return out;
		}
	}
	return NULL;
}

int
errline(const char* path, int lineno, int colno)
{
	char* real = realpath(path, NULL);
	struct stat filestat;
	if (stat(real, &filestat) != 0 || !S_ISREG(filestat.st_mode)) {
		free(real);
		return -1;
	}
	free(real);

	FILE* src = fopen(path, "r");
	if (!src) {
		return -1;
	}
	char* line = NULL;
	size_t len = 0;
	int n = 0;
	while (n < lineno) {
		if (getline(&line, &len, src) == -1) {
			fclose(src);
			if (line) {
				free(line);
			}
			return -1;
		}
		n += 1;
	}
	if (line) {
		bool color = true;
		const char* no_color = getenv("NO_COLOR");
		const char* harec_color = getenv("HAREC_COLOR");
		if (!isatty(fileno(stderr))) {
			color = false;
		}
		if (no_color != NULL) {
			if (0 < strlen(no_color)) {
				color = false;
			}
		}
		if (harec_color != NULL) {
			if (strcmp(harec_color, "0") == 0) {
				color = false;
			} else {
				color = true;
			}
		}
		fprintf(stderr, "\n%d |\t%s", lineno, line);
		for (int i = lineno; 1 <= i; i /= 10) {
			fputc(' ', stderr);
		}
		fputs(" |\t", stderr);
		for (int i = 1; i < colno; i++) {
			fputc(' ', stderr);
		}
		if (color) {
			fputs("\x1b[31m^\x1b[0m\n\n", stderr);
		} else {
			fputs("^\n\n", stderr);
		}
		free(line);
	}
	fclose(src);
	return 0;
}
