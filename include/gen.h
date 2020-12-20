#ifndef HAREC_GEN_H
#define HAREC_GEN_H
#include <stdio.h>

struct unit;

void gen(const struct unit *unit, FILE *out);

#endif
