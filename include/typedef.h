#ifndef HARE_TYPEDEF_H
#define HARE_TYPEDEF_H
#include <stdio.h>

struct unit;

void emit_type(const struct type *type, FILE *out);
void emit_typedefs(struct unit *unit, FILE *out);

#endif
