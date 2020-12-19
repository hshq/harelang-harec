#ifndef HARE_IDENTIFIER_H
#define HARE_IDENTIFIER_H
#include <stddef.h>

struct identifier {
	char *name;
	struct identifier *ns;
};

char *identifier_unparse(const struct identifier *ident);
int identifier_unparse_static(const struct identifier *ident,
		char *buf, size_t len);
void identifier_dup(struct identifier *new, const struct identifier *ident);

#endif
