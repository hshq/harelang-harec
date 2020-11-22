#ifndef HAREC_PARSE_H
#define HAREC_PARSE_H
#include <stdio.h>

struct ast_subunit;
struct lexer;

void parse(struct lexer *lexer, struct ast_subunit *unit);

#endif