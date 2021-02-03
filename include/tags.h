#ifndef HARE_TAGS_H
#define HARE_TAGS_H
#include <stdbool.h>

enum tag_mode {
	TAG_INCLUDE,
	TAG_EXCLUDE,
};

struct build_tags {
	const char *tag;
	enum tag_mode mode;
	struct build_tags *next;
};

// Returns NULL on invalid syntax
struct build_tags *parse_tags(char *input);

bool tag_enabled(struct build_tags *tags, const char *tag);

#endif
