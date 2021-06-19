#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "check.h"
#include "identifier.h"
#include "lex.h"
#include "parse.h"
#include "scope.h"
#include "type_store.h"
#include "typedef.h"

void test(struct context *ctx, char *expected, char *input) {
	builtin_types_init();

	const struct type *etype = NULL;
	if (strlen(expected) != 0) {
		FILE *ebuf = fmemopen(expected, strlen(expected), "r");
		struct lexer elex;
		lex_init(&elex, ebuf, "<expected>");
		struct ast_type *eatype = parse_type(&elex);
		etype = type_store_lookup_atype(ctx->store, eatype);
	}

	FILE *ibuf = fmemopen(input, strlen(input), "r");
	struct lexer ilex;
	lex_init(&ilex, ibuf, "<input>");
	struct ast_expression *iaexpr = parse_expression(&ilex);
	struct expression iexpr = {0};
	struct errors *errors = check_expression(ctx, iaexpr, &iexpr, NULL,
		NULL);

	if (etype == NULL) {
		assert(errors != NULL);
		return;
	}

	struct errors *error = errors;
	while (error && error->prev) {
		error = error->prev;
	}
	while (error) {
		fprintf(stderr, "Error %s:%d:%d: %s\n", error->loc.path,
			error->loc.lineno, error->loc.colno, error->msg);
		struct errors *next = error->next;
		free(error);
		error = next;
	}
	if (errors) {
		exit(EXIT_FAILURE);
	}

	if (etype->id != iexpr.result->id) {
		fprintf(stderr, "Expected expression %s to have type ", input);
		emit_type(etype, stderr);
		fprintf(stderr, ", got ");
		emit_type(iexpr.result, stderr);
		fprintf(stderr, "\n");
		exit(EXIT_FAILURE);
	}
}

int main(void) {
	struct context ctx = {0};
	static struct type_store ts = {0};
	struct modcache *modcache[MODCACHE_BUCKETS];
	memset(modcache, 0, sizeof(modcache));
	ctx.is_test = false;
	ctx.store = &ts;
	ctx.store->check_context = &ctx;
	ctx.modcache = modcache;
	ctx.unit = scope_push(&ctx.scope);

	test(&ctx, "(int | void)", "if (true) 0: int else void: void");
	test(&ctx, "(nullable *int | void)",
		"if (true) null: *int "
		"else if (true) null: nullable *int "
		"else if (true) null");
	test(&ctx, "(nullable *int | void)",
		"match (0u8: (u8 | u16 | u32 | u64)) { "
			"u8 => null: *int, "
			"u16 => null: nullable *int, "
			"u32 => null, "
			"u64 => void, "
		"}");
	test(&ctx, "(nullable *int | void)",
		"switch (0) { "
			"42 => null: *int, "
			"69 => null: nullable *int, "
			"1337 => null, "
			"* => void, "
		"};");

	// if, match, and switch all use the same code for reduction, so we
	// don't need to rigorously test all three

	test(&ctx, "nullable *int",
		"if (true) null: *int "
		"else null");
	test(&ctx, "nullable *int",
		"if (true) null: *int "
		"else null: nullable *int");
	test(&ctx, "(*int | const nullable *int)",
		"if (true) null: *int "
		"else null: const nullable *int");

	test(&ctx, "",
		"if (true) null "
		"else if (true) null: *int "
		"else null: *void");
	test(&ctx, "",
		"if (true) null "
		"else void");
}
