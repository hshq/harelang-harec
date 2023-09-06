#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "identifier.h"
#include "util.h"

uint32_t
identifier_hash(uint32_t init, const struct identifier *ident)
{
	init = fnv1a_s(init, ident->name);
	init = fnv1a(init, 0);
	if (ident->ns) {
		init = identifier_hash(init, ident->ns);
	}
	return init;
}

static void
identifier_unparse_ex(const struct identifier *ident, const char *delim,
	size_t delimlen, char **buf, size_t *len, size_t *cap)
{
	if (ident->ns) {
		identifier_unparse_ex(ident->ns, delim,
			delimlen, buf, len, cap);
		memcpy(*buf + *len, delim, delimlen);
		*len += delimlen;
	}
	size_t namelen = strlen(ident->name);
	if (*len + namelen + delimlen > *cap) {
		*cap += namelen + delimlen;
		*buf = xrealloc(*buf, *cap);
	}
	memcpy(*buf + *len, ident->name, namelen + 1);
	*len += namelen;
}

char *
identifier_unparse(const struct identifier *ident)
{
	size_t len = 0;
	size_t cap = strlen(ident->name) + 1;
	char *buf = xcalloc(cap, sizeof(char));
	identifier_unparse_ex(ident, "::", 2, &buf, &len, &cap);
	return buf;
}

int
identifier_unparse_static(const struct identifier *ident, char *buf, size_t len)
{
	assert(len < INT_MAX);
	if (ident->ns) {
		int prefix = identifier_unparse_static(ident->ns, buf, len);
		int n = snprintf(&buf[prefix], len - prefix,
				"::%s", ident->name);
		if (n >= (int)len) {
			buf[len - 1] = '\0';
		}
		return prefix + n;
	}
	int n = snprintf(buf, len, "%s", ident->name);
	if (n >= (int)len) {
		buf[len - 1] = '\0';
	}
	return n;
}

char *
ident_to_sym(const struct identifier *ident)
{
	size_t len = 0;
	size_t cap = strlen(ident->name) + 1;
	char *buf = xcalloc(cap, sizeof(char));
	identifier_unparse_ex(ident, ".", 1, &buf, &len, &cap);
	return buf;
}

void
identifier_dup(struct identifier *new, const struct identifier *ident)
{
	assert(ident && new);
	new->name = xstrdup(ident->name);
	if (ident->ns) {
		new->ns = xcalloc(1, sizeof(struct identifier));
		identifier_dup(new->ns, ident->ns);
	} else {
		new->ns = NULL;
	}
}

bool
identifier_eq(const struct identifier *a, const struct identifier *b)
{
	if (!a && !b) {
		return true;
	} else if ((!a && b) || (a && !b)) {
		return false;
	}
	if (strcmp(a->name, b->name) != 0) {
		return false;
	}
	return identifier_eq(a->ns, b->ns);
}
