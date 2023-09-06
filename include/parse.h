#ifndef HAREC_PARSE_H
#define HAREC_PARSE_H
#include <stdio.h>

struct ast_expression;
struct ast_subunit;
struct ast_type;
struct lexer;

void parse(struct lexer *lexer, struct ast_subunit *unit);
bool parse_identifier(struct lexer *lexer, struct identifier *ident, bool trailing);
struct ast_type *parse_type(struct lexer *lexer);
struct ast_expression *parse_expression(struct lexer *lexer);

#endif
