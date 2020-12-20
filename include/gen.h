#ifndef HAREC_GEN_H
#define HAREC_GEN_H
#include <stdio.h>

struct unit;
struct qbe_program;

void gen(const struct unit *unit, struct qbe_program *out);

#endif
