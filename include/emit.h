#ifndef HAREC_EMIT_H
#define HAREC_EMIT_H
#include <stdio.h>

struct qbe_program;
void emit(const struct qbe_program *program, FILE *out);

#endif
