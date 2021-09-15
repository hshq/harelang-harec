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
	ctx->errors = NULL;
	ctx->next = &ctx->errors;

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
	check_expression(ctx, iaexpr, &iexpr, NULL);

	if (etype == NULL) {
		assert(ctx->errors != NULL);
		return;
	}

	struct errors *error = ctx->errors;
	while (error) {
		fprintf(stderr, "Error %s:%d:%d: %s\n", error->loc.path,
			error->loc.lineno, error->loc.colno, error->msg);
		struct errors *next = error->next;
		free(error);
		error = next;
	}
	if (ctx->errors) {
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
	ctx.unit = scope_push(&ctx.scope, SCOPE_UNIT);

	test(&ctx, "(int | void)", "if (true) 0: int else void: void");
	test(&ctx, "(nullable *int | void)",
		"if (true) null: *int "
		"else if (true) null: nullable *int "
		"else if (true) null");
	test(&ctx, "(nullable *int | void)",
		"match (0u8: (u8 | u16 | u32 | u64)) { "
		"case u8 => "
		"	yield null: *int; "
		"case u16 => "
		"	yield null: nullable *int; "
		"case u32 => "
		"	yield null; "
		"case u64 => "
		"	yield;"
		"}");
	test(&ctx, "(nullable *int | void)",
		"switch (0) { "
		"case 42 => "
		"	yield null: *int;"
		"case 69 => "
		"	yield null: nullable *int;"
		"case 1337 => "
		"	yield null;"
		"case => "
		"	yield;"
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
	test(&ctx, "const rune",
		"if (true) 'a' "
		"else 'a': const rune");
	test(&ctx, "const rune",
		"if (true) 'a': const rune "
		"else 'a'");
	test(&ctx, "(*int | const nullable *int)",
		"if (true) null: *int "
		"else if (true) null: const nullable *int "
		"else null: nullable *int");

	test(&ctx, "",
		"if (true) null "
		"else if (true) null: *int "
		"else null: *void");
	test(&ctx, "",
		"if (true) null "
		"else void");
}
