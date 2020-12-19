#ifndef HARE_TRACE_H
#define HARE_TRACE_H

enum trace_sys {
	TR_LEX,
	TR_PARSE,
	TR_SCAN,
	TR_CHECK,
	TR_MAX,
};

void trace(enum trace_sys sys, const char *fmt, ...);
void trenter(enum trace_sys sys, const char *fmt, ...);
void trleave(enum trace_sys sys, const char *fmt, ...);

#endif
