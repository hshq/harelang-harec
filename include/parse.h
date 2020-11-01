#ifndef HAREC_PARSE_H
#define HAREC_PARSE_H
#include <stdio.h>

struct ast_unit;
struct identifier;
struct lexer;

void parse(struct lexer *lexer, struct identifier *ns, struct ast_unit *unit);

#endif
