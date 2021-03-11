#ifndef HARE_IDENTIFIER_H
#define HARE_IDENTIFIER_H
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

struct identifier {
	char *name;
	struct identifier *ns;
};

uint32_t identifier_hash(uint32_t init, const struct identifier *ident);
char *identifier_unparse(const struct identifier *ident);
int identifier_unparse_static(
	const struct identifier *ident, char *buf, size_t len);
const char *ident_to_path(const struct identifier *ident);
char *ident_to_sym(const struct identifier *ident);
void identifier_dup(struct identifier *new, const struct identifier *ident);
bool identifier_eq(const struct identifier *a, const struct identifier *b);

#endif
