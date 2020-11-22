#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "trace.h"

void
trace(const char *sys, const char *fmt, ...)
{
	char *t = getenv("HA_TRACE");
	if (!t || !strstr(t, sys)) {
		return;
	}
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "[%10s] ", sys);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}
