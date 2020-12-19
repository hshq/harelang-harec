#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "trace.h"

static const char *sysname[] = {
	[TR_LEX] = "lex",
	[TR_PARSE] = "parse",
	[TR_SCAN] = "scan",
	[TR_CHECK] = "check",
};

static int depth[] = {
	[TR_LEX] = 0,
	[TR_PARSE] = 0,
	[TR_SCAN] = 0,
	[TR_CHECK] = 0,
};

static void
va_trace(enum trace_sys sys, const char *fmt, va_list ap)
{
	assert(sys < TR_MAX);
	char *t = getenv("HA_TRACE");
	if (!t || !strstr(t, sysname[sys])) {
		return;
	}
	fprintf(stderr, "[%10s] ", sysname[sys]);
	for (int i = 0; i < depth[sys]; ++i) {
		fprintf(stderr, " ");
	}
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
}

void
trace(enum trace_sys sys, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	va_trace(sys, fmt, ap);
	va_end(ap);
}

void
trenter(enum trace_sys sys, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	va_trace(sys, fmt, ap);
	va_end(ap);
	++depth[sys];
}

void
trleave(enum trace_sys sys, const char *fmt, ...)
{
	--depth[sys];
	if (fmt == NULL) {
		return;
	}

	va_list ap;
	va_start(ap, fmt);
	va_trace(sys, fmt, ap);
	va_end(ap);
}
