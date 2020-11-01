#ifndef HARE_IDENTIFIER_H
#define HARE_IDENTIFIER_H

struct identifier {
	char *name;
	struct identifier *ns;
};

#endif
