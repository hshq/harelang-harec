#ifndef HARE_TYPEDEF_H
#define HARE_TYPEDEF_H
#include <stdio.h>

struct type;
struct unit;

void emit_type(const struct type *type, FILE *out);
void emit_typedefs(const struct unit *unit, FILE *out);

#endif
