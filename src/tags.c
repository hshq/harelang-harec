#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include "tags.h"
#include "util.h"

struct build_tags *
parse_tags(char *input)
{
	struct build_tags *tag, **next = &tag;
	while (input[0]) {
		enum tag_mode mode;
		switch (input[0]) {
		case '+':
			mode = TAG_INCLUDE;
			break;
		case '-':
			mode = TAG_EXCLUDE;
			break;
		default:
			return NULL;
		}
		++input;

		char c;
		char *tok = NULL;
		char *p = strchr(input, '+'), *m = strchr(input, '-');
		if (p && !m) {
			tok = p;
		} else if (m && !p) {
			tok = m;
		} else if (m && p) {
			tok = m < p ? m : p;
		}

		if (tok) {
			c = *tok;
			*tok = '\0';
		}

		tag = *next = xcalloc(1, sizeof(struct build_tags));
		tag->tag = strdup(input);
		tag->mode = mode;
		next = &tag->next;

		if (tok) {
			*tok = c;
		}

		input += strlen(tag->tag);
	}
	return tag;
}

bool
tag_enabled(struct build_tags *tags, const char *tag)
{
	while (tags) {
		if (strcmp(tags->tag, tag) == 0) {
			return tags->mode == TAG_INCLUDE;
		}
		tags = tags->next;
	}
	return false;
}
