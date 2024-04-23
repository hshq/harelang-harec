#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include "ast.h"
#include "identifier.h"
#include "lex.h"
#include "parse.h"
#include "types.h"
#include "utf8.h"
#include "util.h"

static noreturn void
error(struct location loc, const char *fmt, ...)
{
	xfprintf(stderr, "%s:%d:%d: ", sources[loc.file],
			loc.lineno, loc.colno);

	va_list ap;
	va_start(ap, fmt);
	xvfprintf(stderr, fmt, ap);
	va_end(ap);

	xfprintf(stderr, "\n");
	errline(loc);
	exit(EXIT_PARSE);
}

static void
synassert_msg(bool cond, const char *msg, struct token *tok)
{
	if (!cond) {
		error(tok->loc, "syntax error: %s (found '%s')",
			msg, token_str(tok));
	}
}

static noreturn void
vsynerr(struct token *tok, va_list ap)
{
	enum lexical_token t = va_arg(ap, enum lexical_token);
	
	xfprintf(stderr,
		"%s:%d:%d: syntax error: expected ",
		sources[tok->loc.file], tok->loc.lineno, tok->loc.colno);

	while (t != T_EOF) {
		if (t == T_NUMBER || t == T_NAME) {
			xfprintf(stderr, "%s", lexical_token_str(t));
		} else {
			xfprintf(stderr, "'%s'", lexical_token_str(t));
		}
		t = va_arg(ap, enum lexical_token);
		xfprintf(stderr, ", ");
	}

	xfprintf(stderr,
		"found '%s'\n",
		token_str(tok));

	errline(tok->loc);
	exit(EXIT_PARSE);
}

static noreturn void
synerr(struct token *tok, ...)
{
	va_list ap;
	va_start(ap, tok);
	vsynerr(tok, ap);
	va_end(ap);
}

static void
synassert(bool cond, struct token *tok, ...)
{
	if (!cond) {
		va_list ap;
		va_start(ap, tok);
		vsynerr(tok, ap);
		va_end(ap);
	}
}

static void
want(struct lexer *lexer, enum lexical_token ltok, struct token *tok)
{
	struct token _tok = {0};
	struct token *out = tok ? tok : &_tok;
	lex(lexer, out);
	synassert(out->token == ltok, out, ltok, T_EOF);
	if (!tok) {
		token_finish(out);
	}
}

static struct ast_expression *
mkexpr(struct location loc)
{
	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	exp->loc = loc;
	return exp;
}

static struct ast_type *
mktype(struct location loc)
{
	struct ast_type *t = xcalloc(1, sizeof(struct ast_type));
	t->loc = loc;
	return t;
}

static struct ast_function_parameters *
mkfuncparams(struct location loc)
{
	struct ast_function_parameters *p =
		xcalloc(1, sizeof(struct ast_function_parameters));
	p->loc = loc;
	return p;
}

static struct ast_expression *parse_statement(struct lexer *lexer);

bool
parse_identifier(struct lexer *lexer, struct identifier *ident, bool trailing)
{
	struct token tok = {0};
	struct location loc = {0};
	struct identifier *i = ident;
	*ident = (struct identifier){0};
	size_t len = 0;
	bool found_trailing = false;
	while (!i->name) {
		switch (lex(lexer, &tok)) {
		case T_NAME:
			len += strlen(tok.name);
			i->name = xstrdup(tok.name);
			if (loc.file == 0) {
				loc = tok.loc;
			}
			token_finish(&tok);
			break;
		default:
			synassert(trailing && i->ns, &tok, T_NAME, T_EOF);
			unlex(lexer, &tok);
			struct identifier *ns = i->ns;
			*i = *ns;
			free(ns);
			found_trailing = true;
			continue;
		}

		struct identifier *ns;
		switch (lex(lexer, &tok)) {
		case T_DOUBLE_COLON:
			len++;
			ns = xcalloc(1, sizeof(struct identifier));
			*ns = *i;
			i->ns = ns;
			i->name = NULL;
			break;
		default:
			unlex(lexer, &tok);
			break;
		}
	}

	if (len > IDENT_MAX) {
		error(loc, "identifier exceeds maximum length");
	}
	return found_trailing;
}

static void
parse_import_members(struct lexer *lexer, struct ast_import_members **members)
{
	struct token tok = {0};
	while (true) {
		*members = xcalloc(1, sizeof(struct ast_import_members));
		want(lexer, T_NAME, &tok);
		(*members)->loc = tok.loc;
		(*members)->name = tok.name;
		members = &(*members)->next;

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			break;
		case T_RBRACE:
			return;
		default:
			synerr(&tok, T_COMMA, T_RBRACE, T_EOF);
		}
		switch (lex(lexer, &tok)) {
		case T_NAME:
			unlex(lexer, &tok);
			break;
		case T_RBRACE:
			return;
		default:
			synerr(&tok, T_NAME, T_RBRACE, T_EOF);
		}
	}
}

static void
parse_import(struct lexer *lexer, struct ast_imports *import)
{
	struct token tok = {0};
	import->mode = IMPORT_NORMAL;
	bool trailing_colon = parse_identifier(lexer, &import->ident, true);
	if (trailing_colon) {
		switch (lex(lexer, &tok)) {
		case T_LBRACE:
			import->mode = IMPORT_MEMBERS;
			parse_import_members(lexer, &import->members);
			break;
		case T_TIMES:
			import->mode = IMPORT_WILDCARD;
			break;
		default:
			synerr(&tok, T_LBRACE, T_TIMES, T_EOF);
		}
	} else if (import->ident.ns == NULL) {
		switch (lex(lexer, &tok)) {
		case T_EQUAL:
			import->mode = IMPORT_ALIAS;
			import->alias = import->ident.name;
			parse_identifier(lexer, &import->ident, false);
			break;
		default:
			unlex(lexer, &tok);
			break;
		}
	}
}

static void
parse_imports(struct lexer *lexer, struct ast_subunit *subunit)
{
	struct token tok = {0};
	struct ast_imports **next = &subunit->imports;

	bool more = true;
	while (more) {
		struct ast_imports *imports;
		switch (lex(lexer, &tok)) {
		case T_USE:
			imports = xcalloc(1, sizeof(struct ast_imports));
			parse_import(lexer, imports);
			want(lexer, T_SEMICOLON, NULL);
			*next = imports;
			next = &imports->next;
			break;
		default:
			unlex(lexer, &tok);
			more = false;
			break;
		}
	}
}

static void
parse_parameter_list(struct lexer *lexer, struct ast_function_type *type)
{
	struct token tok = {0}, tok2 = {0};
	want(lexer, T_LPAREN, NULL);
	type->params = mkfuncparams(lexer->loc);
	struct ast_function_parameters **next = &type->params;
	for (;;) {
		switch (lex(lexer, &tok)) {
		case T_NAME:
			// Assumes ownership of tok.name.
			switch (lex(lexer, &tok2)) {
			case T_COLON:
				(*next)->name = tok.name;
				(*next)->type = parse_type(lexer);
				break;
			case T_DOUBLE_COLON:
				(*next)->type = parse_type(lexer);
				synassert((*next)->type->storage == STORAGE_ALIAS,
						&tok, T_NAME, T_EOF);
				struct identifier *ident =
					xcalloc(1, sizeof(struct identifier));
				struct identifier *ns;
				ident->name = tok.name;
				for (ns = &(*next)->type->alias; ns->ns; ns = ns->ns);
				ns->ns = ident;
				break;
			default:
				unlex(lexer, &tok2);
				(*next)->type = mktype(tok.loc);
				(*next)->type->storage = STORAGE_ALIAS;
				(*next)->type->alias.name = tok.name;
				break;
			}
			break;
		case T_ELLIPSIS:
			free(*next);
			*next = NULL;
			type->variadism = VARIADISM_C;
			want(lexer, T_RPAREN, NULL);
			return;
		case T_RPAREN:
			free(*next);
			*next = NULL;
			return;
		default:
			unlex(lexer, &tok);
			(*next)->type = parse_type(lexer);
			break;
		}

		switch (lex(lexer, &tok)) {
		case T_EQUAL:
			(*next)->default_value = parse_expression(lexer);
			break;
		default:
			unlex(lexer, &tok);
			break;
		}

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			(*next)->next = mkfuncparams(lexer->loc);
			next = &(*next)->next;
			break;
		case T_ELLIPSIS:
			type->variadism = VARIADISM_HARE;
			want(lexer, T_RPAREN, NULL);
			return;
		case T_RPAREN:
			return;
		default:
			synerr(&tok, T_COMMA, T_ELLIPSIS, T_RPAREN, T_EQUAL, T_EOF);
		}
	}
}

static void
parse_prototype(struct lexer *lexer, struct ast_function_type *type)
{
	parse_parameter_list(lexer, type);
	type->result = parse_type(lexer);
}

static enum type_storage
parse_integer_type(struct lexer *lexer)
{
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_I8:
		return STORAGE_I8;
	case T_I16:
		return STORAGE_I16;
	case T_I32:
		return STORAGE_I32;
	case T_I64:
		return STORAGE_I64;
	case T_U8:
		return STORAGE_U8;
	case T_U16:
		return STORAGE_U16;
	case T_U32:
		return STORAGE_U32;
	case T_U64:
		return STORAGE_U64;
	case T_INT:
		return STORAGE_INT;
	case T_UINT:
		return STORAGE_UINT;
	case T_SIZE:
		return STORAGE_SIZE;
	case T_UINTPTR:
		return STORAGE_UINTPTR;
	default:
		assert(0);
	}
}

static struct ast_type *
parse_primitive_type(struct lexer *lexer)
{
	struct token tok = {0};
	struct ast_type *type = mktype(lexer->loc);
	switch (lex(lexer, &tok)) {
	case T_I8:
	case T_I16:
	case T_I32:
	case T_I64:
	case T_U8:
	case T_U16:
	case T_U32:
	case T_U64:
	case T_INT:
	case T_UINT:
	case T_SIZE:
	case T_UINTPTR:
		unlex(lexer, &tok);
		type->storage = parse_integer_type(lexer);
		break;
	case T_RUNE:
		type->storage = STORAGE_RUNE;
		break;
	case T_STR:
		type->storage = STORAGE_STRING;
		break;
	case T_F32:
		type->storage = STORAGE_F32;
		break;
	case T_F64:
		type->storage = STORAGE_F64;
		break;
	case T_BOOL:
		type->storage = STORAGE_BOOL;
		break;
	case T_VOID:
		type->storage = STORAGE_VOID;
		break;
	case T_DONE:
		type->storage = STORAGE_DONE;
		break;
	case T_OPAQUE:
		type->storage = STORAGE_OPAQUE;
		break;
	case T_NEVER:
		type->storage = STORAGE_NEVER;
		break;
	case T_VALIST:
		type->storage = STORAGE_VALIST;
		break;
	default:
		assert(0);
	}
	return type;
}

static void parse_binding_unpack(struct lexer *lexer,
	 struct ast_binding_unpack **next);
static struct ast_expression *parse_binding_list(
		struct lexer *lexer, bool is_static);
static struct ast_expression *parse_object_selector(struct lexer *lexer);

static struct ast_type *
parse_enum_type(struct identifier *ident, struct lexer *lexer)
{
	struct token tok = {0};
	struct ast_type *type = mktype(lexer->loc);
	type->storage = STORAGE_ENUM;
	identifier_dup(&type->alias, ident);
	struct ast_enum_field **next = &type->_enum.values;
	switch (lex(lexer, &tok)) {
	case T_LBRACE:
		type->_enum.storage = STORAGE_INT;
		unlex(lexer, &tok);
		break;
	case T_I8:
	case T_I16:
	case T_I32:
	case T_I64:
	case T_U8:
	case T_U16:
	case T_U32:
	case T_U64:
	case T_INT:
	case T_UINT:
	case T_SIZE:
	case T_UINTPTR:
		unlex(lexer, &tok);
		type->_enum.storage = parse_integer_type(lexer);
		break;
	case T_RUNE:
		type->_enum.storage = STORAGE_RUNE;
		break;
	default:
		synassert_msg(false, "Enum storage must be an integer or rune", &tok);
	}
	want(lexer, T_LBRACE, NULL);
	while (tok.token != T_RBRACE) {
		*next = xcalloc(1, sizeof(struct ast_enum_field));
		want(lexer, T_NAME, &tok);
		(*next)->name = tok.name;
		(*next)->loc = tok.loc;
		if (lex(lexer, &tok) == T_EQUAL) {
			(*next)->value = parse_expression(lexer);
		} else {
			unlex(lexer, &tok);
		}
		next = &(*next)->next;
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			if (lex(lexer, &tok) != T_RBRACE) {
				unlex(lexer, &tok);
			}
			break;
		case T_RBRACE:
			break;
		default:
			synerr(&tok, T_COMMA, T_RBRACE, T_EOF);
		}
	}
	return type;
}

static struct ast_type *
parse_struct_union_type(struct lexer *lexer)
{
	struct token tok = {0};
	struct ast_type *type = mktype(lexer->loc);
	struct ast_struct_union_field *next = &type->struct_union.fields;
	switch (lex(lexer, &tok)) {
	case T_STRUCT:
		type->storage = STORAGE_STRUCT;
		break;
	case T_UNION:
		type->storage = STORAGE_UNION;
		break;
	default:
		synerr(&tok, T_STRUCT, T_UNION, T_EOF);
		break;
	}
	if (lex(lexer, &tok) == T_ATTR_PACKED) {
		type->struct_union.packed = true;
	} else {
		unlex(lexer, &tok);
	}
	want(lexer, T_LBRACE, NULL);
	while (tok.token != T_RBRACE) {
		if (lex(lexer, &tok) == T_ATTR_OFFSET) {
			want(lexer, T_LPAREN, NULL);
			next->offset = parse_expression(lexer);
			want(lexer, T_RPAREN, NULL);
		} else {
			unlex(lexer, &tok);
		}

		char *name;
		switch (lex(lexer, &tok)) {
		case T_NAME:
			name = tok.name;
			struct location loc = tok.loc;
			switch (lex(lexer, &tok)) {
			case T_COLON:
				next->name = name;
				next->type = parse_type(lexer);
				break;
			case T_DOUBLE_COLON:
				next->type = mktype(loc);
				next->type->storage = STORAGE_ALIAS;
				next->type->unwrap = false;
				parse_identifier(lexer, &next->type->alias, false);
				struct identifier *i = &next->type->alias;
				while (i->ns != NULL) {
					i = i->ns;
				}
				i->ns = xcalloc(1, sizeof(struct identifier));
				i->ns->name = name;
				break;
			default:
				unlex(lexer, &tok);
				next->type = mktype(loc);
				next->type->storage = STORAGE_ALIAS;
				next->type->alias.name = name;
				next->type->unwrap = false;
				break;
			}
			break;
		case T_STRUCT:
		case T_UNION:
			unlex(lexer, &tok);
			next->name = NULL;
			next->type = parse_type(lexer);
			break;
		default:
			synerr(&tok, T_NAME, T_STRUCT, T_UNION, T_EOF);
		}
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			if (lex(lexer, &tok) != T_RBRACE) {
				unlex(lexer, &tok);
				next->next = xcalloc(1,
					sizeof(struct ast_struct_union_field));
				next = next->next;
			}
			break;
		case T_RBRACE:
			break;
		default:
			synerr(&tok, T_COMMA, T_RBRACE, T_EOF);
		}
	}
	return type;
}

static struct ast_type *
parse_tagged_type(struct lexer *lexer, struct ast_type *first)
{
	struct ast_type *type = mktype(first->loc);
	type->storage = STORAGE_TAGGED;
	struct ast_tagged_union_type *next = &type->tagged;
	next->type = first;
	struct token tok = {0};
	while (tok.token != T_RPAREN) {
		next->next = xcalloc(1, sizeof(struct ast_tagged_union_type));
		next = next->next;
		next->type = parse_type(lexer);
		switch (lex(lexer, &tok)) {
		case T_BOR:
			if (lex(lexer, &tok) != T_RPAREN) {
				unlex(lexer, &tok);
			}
			break;
		case T_RPAREN:
			break;
		default:
			synerr(&tok, T_BOR, T_RPAREN, T_EOF);
		}
	}
	return type;
}

static struct ast_type *
parse_tuple_type(struct lexer *lexer, struct ast_type *first)
{
	struct ast_type *type = mktype(first->loc);
	type->storage = STORAGE_TUPLE;
	struct ast_tuple_type *next = &type->tuple;
	next->type = first;
	struct token tok = {0};
	while (tok.token != T_RPAREN) {
		next->next = xcalloc(1, sizeof(struct ast_tuple_type));
		next = next->next;
		next->type = parse_type(lexer);
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			if (lex(lexer, &tok) != T_RPAREN) {
				unlex(lexer, &tok);
			}
			break;
		case T_RPAREN:
			break;
		default:
			synerr(&tok, T_COMMA, T_RPAREN, T_EOF);
		}
	}
	return type;
}

static struct ast_type *
parse_tagged_or_tuple_type(struct lexer *lexer)
{
	struct ast_type *type = parse_type(lexer);
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_BOR:
		return parse_tagged_type(lexer, type);
	case T_COMMA:
		return parse_tuple_type(lexer, type);
	default:
		synerr(&tok, T_BOR, T_COMMA, T_EOF);
	}
	assert(0); // Unreachable
}

struct ast_type *
parse_type(struct lexer *lexer)
{
	struct token tok = {0};
	uint32_t flags = 0;
	switch (lex(lexer, &tok)) {
	case T_CONST:
		flags |= TYPE_CONST;
		break;
	default:
		unlex(lexer, &tok);
		break;
	}
	switch (lex(lexer, &tok)) {
	case T_LNOT:
		flags |= TYPE_ERROR;
		break;
	default:
		unlex(lexer, &tok);
		break;
	}
	struct ast_type *type = NULL;
	bool nullable = false, unwrap = false;
	switch (lex(lexer, &tok)) {
	case T_BOOL:
	case T_F32:
	case T_F64:
	case T_I16:
	case T_I32:
	case T_I64:
	case T_I8:
	case T_INT:
	case T_NEVER:
	case T_OPAQUE:
	case T_RUNE:
	case T_SIZE:
	case T_STR:
	case T_U16:
	case T_U32:
	case T_U64:
	case T_U8:
	case T_UINT:
	case T_UINTPTR:
	case T_VALIST:
	case T_VOID:
	case T_DONE:
		unlex(lexer, &tok);
		type = parse_primitive_type(lexer);
		break;
	case T_NULLABLE:
		nullable = true;
		want(lexer, T_TIMES, NULL);
		/* fallthrough */
	case T_TIMES:
		type = mktype(lexer->loc);
		type->storage = STORAGE_POINTER;
		type->pointer.referent = parse_type(lexer);
		if (nullable) {
			type->pointer.flags |= PTR_NULLABLE;
		}
		break;
	case T_STRUCT:
	case T_UNION:
		unlex(lexer, &tok);
		type = parse_struct_union_type(lexer);
		break;
	case T_LPAREN:
		type = parse_tagged_or_tuple_type(lexer);
		break;
	case T_LBRACKET:
		type = mktype(lexer->loc);
		switch (lex(lexer, &tok)) {
		case T_RBRACKET:
			type->storage = STORAGE_SLICE;
			type->slice.members = parse_type(lexer);
			break;
		case T_TIMES:
			type->storage = STORAGE_ARRAY;
			type->array.length = NULL;
			want(lexer, T_RBRACKET, NULL);
			type->array.members = parse_type(lexer);
			break;
		case T_UNDERSCORE:
			type->storage = STORAGE_ARRAY;
			type->array.length = NULL;
			type->array.contextual = true;
			want(lexer, T_RBRACKET, NULL);
			type->array.members = parse_type(lexer);
			break;
		default:
			type->storage = STORAGE_ARRAY;
			unlex(lexer, &tok);
			type->array.length = parse_expression(lexer);
			want(lexer, T_RBRACKET, NULL);
			type->array.members = parse_type(lexer);
			break;
		}
		break;
	case T_FN:
		type = mktype(lexer->loc);
		type->storage = STORAGE_FUNCTION;
		parse_prototype(lexer, &type->func);
		break;
	case T_ELLIPSIS:
		unwrap = true;
		want(lexer, T_NAME, &tok);
		// Fallthrough
	case T_NAME:
		unlex(lexer, &tok);
		type = mktype(lexer->loc);
		type->storage = STORAGE_ALIAS;
		type->unwrap = unwrap;
		parse_identifier(lexer, &type->alias, false);
		break;
	default:
		synassert_msg(false, "expected type", &tok);
		break;
	}
	type->flags |= flags;

	return type;
}

static struct ast_expression *
parse_access(struct lexer *lexer, struct identifier ident)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_ACCESS;
	exp->access.type = ACCESS_IDENTIFIER;
	exp->access.ident = ident;
	return exp;
}

static struct ast_expression *
parse_literal(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_LITERAL;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_TRUE:
		exp->literal.storage = STORAGE_BOOL;
		exp->literal.bval = true;
		return exp;
	case T_FALSE:
		exp->literal.storage = STORAGE_BOOL;
		exp->literal.bval = false;
		return exp;
	case T_NULL:
		exp->literal.storage = STORAGE_NULL;
		return exp;
	case T_VOID:
		exp->literal.storage = STORAGE_VOID;
		return exp;
	case T_DONE:
		exp->literal.storage = STORAGE_DONE;
		return exp;
	case T_NUMBER:
		exp->literal.storage = tok.storage;
		break;
	default:
		synerr(&tok, T_NUMBER, T_TRUE,
			T_FALSE, T_NULL, T_VOID, T_EOF);
		break;
	}

	struct location loc = tok.loc;
	switch (tok.storage) {
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_SIZE:
		exp->literal.uval = (uint64_t)tok.uval;
		break;
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_ICONST:
	case STORAGE_INT:
		exp->literal.ival = (int64_t)tok.ival;
		break;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		exp->literal.fval = tok.fval;
		break;
	case STORAGE_RCONST:
		exp->literal.rune = tok.rune;
		break;
	case STORAGE_STRING:
		exp->literal.string.len = tok.string.len;
		exp->literal.string.value = tok.string.value;
		while (lex(lexer, &tok) == T_NUMBER
				&& tok.storage == STORAGE_STRING) {
			size_t len = exp->literal.string.len;
			exp->literal.string.value = xrealloc(
				exp->literal.string.value,
				len + tok.string.len);
			memcpy(exp->literal.string.value + len,
				tok.string.value, tok.string.len);
			exp->literal.string.len += tok.string.len;
		}
		unlex(lexer, &tok);

		// check for invalid UTF-8 (possible when \x is used)
		const char *s = exp->literal.string.value;
		size_t len = exp->literal.string.len;
		while (s - exp->literal.string.value < (ptrdiff_t)len) {
			if (utf8_decode(&s) == UTF8_INVALID) {
				error(loc, "invalid UTF-8 in string literal");
			}
		}
		break;
	case STORAGE_BOOL:
	case STORAGE_NULL:
	case STORAGE_VOID:
	case STORAGE_DONE:
		assert(0); // Handled above
	case STORAGE_ALIAS:
	case STORAGE_ARRAY:
	case STORAGE_ENUM:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_RUNE:
	case STORAGE_SLICE:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
		assert(0); // Handled in a different nonterminal
	case STORAGE_ERROR:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
		assert(0); // Invariant
	}
	return exp;
}

static struct ast_expression *
parse_array_literal(struct lexer *lexer)
{
	struct token tok;
	want(lexer, T_LBRACKET, &tok);

	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_LITERAL;
	exp->literal.storage = STORAGE_ARRAY;

	struct ast_expression_list *item, **next = &exp->literal.array.exprs;

	while (lex(lexer, &tok) != T_RBRACKET) {
		unlex(lexer, &tok);

		item = *next = xcalloc(1, sizeof(struct ast_expression_list));
		item->expr = parse_expression(lexer);
		next = &item->next;

		switch (lex(lexer, &tok)) {
		case T_ELLIPSIS:
			exp->literal.array.expand = true;
			want(lexer, T_RBRACKET, &tok);
			// fallthrough
		case T_RBRACKET:
			unlex(lexer, &tok);
			break;
		case T_COMMA:
			// Move on
			break;
		default:
			synerr(&tok, T_ELLIPSIS, T_COMMA, T_RBRACKET, T_EOF);
		}
	}
	return exp;
}

static struct ast_expression *parse_struct_literal(struct lexer *lexer,
	struct identifier ident);

static struct ast_field_value *
parse_field_value(struct lexer *lexer)
{
	struct ast_field_value *exp =
		xcalloc(1, sizeof(struct ast_field_value));
	char *name;
	struct token tok = {0};
	struct identifier ident = {0};
	struct identifier *i;
	switch (lex(lexer, &tok)) {
	case T_NAME:
		name = tok.name;
		switch (lex(lexer, &tok)) {
		case T_COLON:
			exp->name = name;
			exp->type = parse_type(lexer);
			want(lexer, T_EQUAL, NULL);
			exp->initializer = parse_expression(lexer);
			break;
		case T_EQUAL:
			exp->name = name;
			exp->initializer = parse_expression(lexer);
			break;
		case T_DOUBLE_COLON:
			i = &ident;
			parse_identifier(lexer, i, false);
			while (i->ns != NULL) {
				i = i->ns;
			}
			i->ns = xcalloc(1, sizeof(struct identifier));
			i->ns->name = name;
			exp->initializer = parse_struct_literal(lexer, ident);
			break;
		default:
			unlex(lexer, &tok);
			ident.name = name;
			ident.ns = NULL;
			exp->initializer = parse_struct_literal(lexer, ident);
			break;
		}
		break;
	case T_STRUCT:
		exp->initializer = parse_struct_literal(lexer, ident);
		break;
	default:
		assert(0);
	}
	return exp;
}

static struct ast_expression *
parse_struct_literal(struct lexer *lexer, struct identifier ident)
{
	want(lexer, T_LBRACE, NULL);
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_STRUCT;
	exp->_struct.type = ident;
	struct ast_field_value **next = &exp->_struct.fields;
	struct token tok = {0};
	while (tok.token != T_RBRACE) {
		switch (lex(lexer, &tok)) {
		case T_ELLIPSIS:
			synassert(ident.name != NULL, &tok, T_RBRACE, T_EOF);
			exp->_struct.autofill = true;
			want(lexer, T_RBRACE, &tok);
			unlex(lexer, &tok);
			break;
		case T_NAME:
		case T_STRUCT:
			unlex(lexer, &tok);
			*next = parse_field_value(lexer);
			next = &(*next)->next;
			break;
		default:
			synerr(&tok, T_ELLIPSIS, T_NAME, T_RBRACE,
				T_STRUCT, T_EOF);
			break;
		}
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			if (lex(lexer, &tok) != T_RBRACE) {
				unlex(lexer, &tok);
			}
			break;
		case T_RBRACE:
			break;
		default:
			synerr(&tok, T_COMMA, T_RBRACE, T_EOF);
		}
	}
	return exp;
}

static struct ast_expression *
parse_tuple_expression(struct lexer *lexer, struct ast_expression *first)
{
	struct ast_expression *exp = mkexpr(first->loc);
	exp->type = EXPR_TUPLE;

	bool more = true;
	struct token tok = {0};
	struct ast_expression_tuple *tuple = &exp->tuple;
	tuple->expr = first;
	tuple->next = xcalloc(1, sizeof(struct ast_expression_tuple));
	tuple = tuple->next;

	while (more) {
		tuple->expr = parse_expression(lexer);

		switch (lex(lexer, &tok)) {
		case T_RPAREN:
			more = false;
			break;
		case T_COMMA:
			if (lex(lexer, &tok) == T_RPAREN) {
				more = false;
			} else {
				unlex(lexer, &tok);
				tuple->next = xcalloc(1,
					sizeof(struct ast_expression_tuple));
				tuple = tuple->next;
			}
			break;
		default:
			synerr(&tok, T_RPAREN, T_COMMA, T_EOF);
		}
	}

	return exp;
}

static struct ast_expression *
parse_plain_expression(struct lexer *lexer)
{
	struct token tok = {0};
	struct ast_expression *exp;
	switch (lex(lexer, &tok)) {
	// plain-expression
	case T_NUMBER:
	case T_TRUE:
	case T_FALSE:
	case T_NULL:
	case T_VOID:
	case T_DONE:
		unlex(lexer, &tok);
		return parse_literal(lexer);
	case T_NAME:
		unlex(lexer, &tok);
		struct identifier ident = {0};
		parse_identifier(lexer, &ident, false);
		switch (lex(lexer, &tok)) {
		case T_LBRACE:
			unlex(lexer, &tok);
			return parse_struct_literal(lexer, ident);
		default:
			unlex(lexer, &tok);
			return parse_access(lexer, ident);
		}
		assert(0);
	case T_LBRACKET:
		unlex(lexer, &tok);
		return parse_array_literal(lexer);
	case T_STRUCT:
		ident.name = NULL;
		ident.ns = NULL;
		return parse_struct_literal(lexer, ident);
	// nested-expression
	case T_LPAREN:
		exp = parse_expression(lexer);
		switch (lex(lexer, &tok)) {
		case T_RPAREN:
			return exp;
		case T_COMMA:
			return parse_tuple_expression(lexer, exp);
		default:
			synerr(&tok, T_RPAREN, T_COMMA, T_EOF);
		}
		assert(0); // Unreachable
	// empty block
	case T_RBRACE:
		error(tok.loc, "syntax error: cannot have empty block");
	default:
		synerr(&tok, T_NUMBER, T_NAME,
			T_LBRACKET, T_STRUCT, T_LPAREN, T_EOF);
	}
	assert(0); // Unreachable
}

static void
parse_assertion(struct lexer *lexer, bool is_static,
	struct ast_expression_assert *exp)
{
	exp->is_static = is_static;

	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_ASSERT:
	case T_ABORT:
		break;
	default:
		synerr(&tok, T_ASSERT, T_ABORT, T_EOF);
	}

	switch (tok.token) {
	case T_ASSERT:
		want(lexer, T_LPAREN, &tok);
		exp->cond = parse_expression(lexer);
		if (lex(lexer, &tok) == T_COMMA) {
			exp->message = parse_expression(lexer);
		} else {
			unlex(lexer, &tok);
		}
		want(lexer, T_RPAREN, &tok);
		break;
	case T_ABORT:
		want(lexer, T_LPAREN, &tok);
		if (lex(lexer, &tok) != T_RPAREN) {
			unlex(lexer, &tok);
			exp->message = parse_expression(lexer);
			want(lexer, T_RPAREN, &tok);
		}
		break;
	default:
		assert(0); // Invariant
	}
}

static struct ast_expression *
parse_assertion_expression(struct lexer *lexer, bool is_static)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_ASSERT;
	parse_assertion(lexer, is_static, &exp->assert);
	return exp;
}

static struct ast_expression *
parse_measurement_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_MEASURE;

	struct token tok;
	lex(lexer, &tok);

	want(lexer, T_LPAREN, NULL);
	switch (tok.token) {
	case T_ALIGN:
		exp->measure.op = M_ALIGN;
		exp->measure.type = parse_type(lexer);
		break;
	case T_SIZE:
		exp->measure.op = M_SIZE;
		exp->measure.type = parse_type(lexer);
		break;
	case T_LEN:
		exp->measure.op = M_LEN;
		exp->measure.value = parse_expression(lexer);
		break;
	case T_OFFSET:
		exp->measure.op = M_OFFSET;
		// Let check error out on non-field-accesses
		exp->measure.value = parse_expression(lexer);
		break;
	default:
		synerr(&tok, T_SIZE, T_LEN, T_OFFSET, T_EOF);
	}

	want(lexer, T_RPAREN, NULL);
	return exp;
}

static struct ast_expression *
parse_call_expression(struct lexer *lexer, struct ast_expression *lvalue)
{
	struct token tok;
	want(lexer, T_LPAREN, &tok);

	struct ast_expression *expr = mkexpr(lexer->loc);
	expr->type = EXPR_CALL;
	expr->call.lvalue = lvalue;

	struct ast_call_argument *arg, **next = &expr->call.args;
	while (lex(lexer, &tok) != T_RPAREN) {
		unlex(lexer, &tok);
		arg = *next = xcalloc(1, sizeof(struct ast_call_argument));
		arg->value = parse_expression(lexer);

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			break;
		case T_ELLIPSIS:
			arg->variadic = true;
			want(lexer, T_RPAREN, &tok);
			// fallthrough
		case T_RPAREN:
			unlex(lexer, &tok);
			break;
		default:
			synerr(&tok, T_COMMA, T_RPAREN, T_ELLIPSIS, T_EOF);
		}

		next = &arg->next;
	}
	return expr;
}

static struct ast_expression *
parse_index_slice_expression(struct lexer *lexer, struct ast_expression *lvalue)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	struct ast_expression *start = NULL, *end = NULL;
	struct token tok;
	want(lexer, T_LBRACKET, &tok);

	bool is_slice = false;
	switch (lex(lexer, &tok)) {
	case T_DOUBLE_DOT:
		is_slice = true;
		break;
	default:
		unlex(lexer, &tok);
		start = parse_expression(lexer);
		break;
	}

	switch (lex(lexer, &tok)) {
	case T_DOUBLE_DOT:
		is_slice = true;
		break;
	case T_RBRACKET:
		break;
	default:
		if (is_slice) {
			unlex(lexer, &tok);
			break;
		}
		synerr(&tok, T_DOUBLE_DOT, T_RBRACKET, T_EOF);
		break;
	}

	if (!is_slice) {
		exp->type = EXPR_ACCESS;
		exp->access.type = ACCESS_INDEX;
		exp->access.array = lvalue;
		exp->access.index = start;
		return exp;
	} else if (tok.token == T_RBRACKET) {
		unlex(lexer, &tok);
	}

	switch (lex(lexer, &tok)) {
	case T_RBRACKET:
		break;
	default:
		unlex(lexer, &tok);
		end = parse_expression(lexer);
		want(lexer, T_RBRACKET, &tok);
		break;
	}

	exp->type = EXPR_SLICE;
	exp->slice.object = lvalue;
	exp->slice.start = start;
	exp->slice.end = end;
	return exp;
}

static struct ast_expression *
parse_allocation_expression(struct lexer *lexer)
{
	struct ast_expression *exp = NULL;
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_ALLOC:
		exp = mkexpr(tok.loc);
		exp->type = EXPR_ALLOC;
		exp->alloc.kind = ALLOC_OBJECT;
		want(lexer, T_LPAREN, NULL);
		exp->alloc.init = parse_expression(lexer);
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			// alloc(init, cap)
			exp->alloc.cap = parse_expression(lexer);
			exp->alloc.kind = ALLOC_WITH_CAP;
			want(lexer, T_RPAREN, NULL);
			break;
		case T_ELLIPSIS:
			// alloc(init...)
			exp->alloc.kind = ALLOC_COPY;
			want(lexer, T_RPAREN, NULL);
			break;
		case T_RPAREN:
			// alloc(init)
			break;
		default:
			synerr(&tok, T_COMMA, T_RPAREN, T_ELLIPSIS, T_EOF);
		}
		break;
	case T_FREE:
		exp = mkexpr(tok.loc);
		exp->type = EXPR_FREE;
		want(lexer, T_LPAREN, NULL);
		exp->free.expr = parse_expression(lexer);
		want(lexer, T_RPAREN, NULL);
		break;
	default:
		assert(0);
	}
	return exp;
}

static struct ast_expression *
parse_append_insert(struct lexer *lexer, struct location loc,
		bool is_static, enum expr_type etype)
{
	struct token tok = {0};
	struct ast_expression *expr = mkexpr(loc);
	expr->type = etype;

	want(lexer, T_LPAREN, NULL);
	expr->append.object = parse_object_selector(lexer);
	if (etype == EXPR_INSERT) {
		synassert_msg(expr->append.object->access.type == ACCESS_INDEX,
				"expected indexing expression", &tok);
	}
	want(lexer, T_COMMA, NULL);
	expr->append.value = parse_expression(lexer);
	expr->append.is_static = is_static;

	switch (lex(lexer, &tok)) {
	case T_RPAREN:
		// This space deliberately left blank
		break;
	case T_ELLIPSIS:
		expr->append.is_multi = true;
		want(lexer, T_RPAREN, NULL);
		break;
	case T_COMMA:
		expr->append.length = parse_expression(lexer);
		want(lexer, T_RPAREN, NULL);
		break;
	default:
		synerr(&tok, T_RPAREN, T_ELLIPSIS, T_COMMA, T_EOF);
	}

	return expr;
}

static struct ast_expression *
parse_delete(struct lexer *lexer, struct location loc, bool is_static)
{
	struct ast_expression *exp = mkexpr(loc);
	exp->type = EXPR_DELETE;
	want(lexer, T_LPAREN, NULL);
	exp->delete.expr = parse_expression(lexer);
	exp->delete.is_static = is_static;
	want(lexer, T_RPAREN, NULL);
	return exp;
}

static struct ast_expression *
parse_slice_mutation(struct lexer *lexer, bool is_static)
{
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_APPEND:
	case T_INSERT:
		return parse_append_insert(lexer, tok.loc, is_static,
			tok.token == T_APPEND ? EXPR_APPEND : EXPR_INSERT);
	case T_DELETE:
		return parse_delete(lexer, tok.loc, is_static);
	default:
		abort(); // Invariant
	}
}

static struct ast_expression *
parse_static_expression(struct lexer *lexer, bool allowbinding)
{
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_LET:
	case T_CONST:
		synassert(allowbinding, &tok, T_ABORT, T_ASSERT, T_APPEND,
			T_INSERT, T_DELETE, T_EOF);
		unlex(lexer, &tok);
		return parse_binding_list(lexer, true);
	case T_ABORT:
	case T_ASSERT:
		unlex(lexer, &tok);
		return parse_assertion_expression(lexer, true);
	case T_APPEND:
	case T_INSERT:
	case T_DELETE:
		unlex(lexer, &tok);
		return parse_slice_mutation(lexer, true);
	default:
		if (allowbinding) {
			synerr(&tok, T_LET, T_CONST, T_ABORT,
				T_ASSERT, T_APPEND, T_INSERT, T_DELETE, T_EOF);
		} else {
			synerr(&tok, T_ABORT, T_ASSERT, T_APPEND,
				T_INSERT, T_DELETE, T_EOF);
		}
	}
	assert(0); // Unreachable
}

static struct ast_expression *
parse_postfix_expression(struct lexer *lexer, struct ast_expression *lvalue)
{
	if (lvalue == NULL) {
		lvalue = parse_plain_expression(lexer);
	}

	struct token tok;
	struct ast_expression *exp;
	switch (lex(lexer, &tok)) {
	case T_LPAREN:
		unlex(lexer, &tok);
		lvalue = parse_call_expression(lexer, lvalue);
		break;
	case T_DOT:
		exp = mkexpr(lexer->loc);
		exp->type = EXPR_ACCESS;

		switch (lex(lexer, &tok)) {
		case T_NAME:
			exp->access.type = ACCESS_FIELD;
			exp->access._struct = lvalue;
			exp->access.field = tok.name;
			break;
		case T_NUMBER:
			exp->access.type = ACCESS_TUPLE;
			exp->access.tuple = lvalue;
			unlex(lexer, &tok);
			exp->access.value = parse_literal(lexer);
			break;
		default:
			synerr(&tok, T_NAME, T_NUMBER, T_EOF);
		}

		lvalue = exp;
		break;
	case T_LBRACKET:
		unlex(lexer, &tok);
		lvalue = parse_index_slice_expression(lexer, lvalue);
		break;
	case T_QUESTION:
	case T_LNOT:
		exp = mkexpr(lexer->loc);
		exp->type = EXPR_PROPAGATE;
		exp->propagate.value = lvalue;
		exp->propagate.abort = tok.token == T_LNOT;
		lvalue = exp;
		break;
	default:
		unlex(lexer, &tok);
		return lvalue;
	}

	return parse_postfix_expression(lexer, lvalue);
}

static enum unarithm_operator
unop_for_token(enum lexical_token tok)
{
	switch (tok) {
	case T_MINUS:	// -
		return UN_MINUS;
	case T_BNOT:	// ~
		return UN_BNOT;
	case T_LNOT:	// !
		return UN_LNOT;
	case T_TIMES:	// *
		return UN_DEREF;
	case T_BAND:	// &
		return UN_ADDRESS;
	default:
		assert(0); // Invariant
	}
	assert(0); // Unreachable
}

static struct ast_expression *
parse_object_selector(struct lexer *lexer)
{
	struct token tok;
	lex(lexer, &tok);
	unlex(lexer, &tok);
	struct ast_expression *exp = parse_postfix_expression(lexer, NULL);
	synassert_msg(exp->type == EXPR_ACCESS,
		"Expected object selector (identifier, indexing, or field access)",
		&tok);
	return exp;
}

static struct ast_expression *
parse_va_expression(struct lexer *lexer)
{
	struct ast_expression *expr;
	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_VASTART:
		expr = mkexpr(lexer->loc);
		expr->type = EXPR_VASTART;
		want(lexer, T_LPAREN, NULL);
		want(lexer, T_RPAREN, NULL);
		return expr;
	case T_VAARG:
		expr = mkexpr(lexer->loc);
		expr->type = EXPR_VAARG;
		want(lexer, T_LPAREN, NULL);
		expr->vaarg.ap = parse_object_selector(lexer);
		want(lexer, T_RPAREN, NULL);
		return expr;
	case T_VAEND:
		expr = mkexpr(lexer->loc);
		expr->type = EXPR_VAEND;
		want(lexer, T_LPAREN, NULL);
		expr->vaarg.ap = parse_object_selector(lexer);
		want(lexer, T_RPAREN, NULL);
		return expr;
	default:
		assert(0);
	}
}

static struct ast_expression *
parse_builtin_expression(struct lexer *lexer)
{
	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_ALLOC:
	case T_FREE:
		unlex(lexer, &tok);
		return parse_allocation_expression(lexer);
	case T_APPEND:
	case T_DELETE:
	case T_INSERT:
		unlex(lexer, &tok);
		return parse_slice_mutation(lexer, false);
	case T_STATIC:
		return parse_static_expression(lexer, false);
	case T_ABORT:
	case T_ASSERT:
		unlex(lexer, &tok);
		return parse_assertion_expression(lexer, false);
	case T_ALIGN:
	case T_SIZE:
	case T_LEN:
	case T_OFFSET:
		unlex(lexer, &tok);
		return parse_measurement_expression(lexer);
	case T_VAARG:
	case T_VAEND:
	case T_VASTART:
		unlex(lexer, &tok);
		return parse_va_expression(lexer);
	default:
		unlex(lexer, &tok);
		break;
	}
	return parse_postfix_expression(lexer, NULL);
}

static struct ast_expression *parse_compound_expression(struct lexer *lexer);
static struct ast_expression *parse_match_expression(struct lexer *lexer);
static struct ast_expression *parse_switch_expression(struct lexer *lexer);

static struct ast_expression *
parse_unary_expression(struct lexer *lexer)
{
	struct token tok;
	struct ast_expression *exp;
	switch (lex(lexer, &tok)) {
	case T_MINUS:	// -
	case T_BNOT:	// ~
	case T_LNOT:	// !
	case T_TIMES:	// *
	case T_BAND:	// &
		exp = mkexpr(lexer->loc);
		exp->type = EXPR_UNARITHM;
		exp->unarithm.op = unop_for_token(tok.token);
		exp->unarithm.operand = parse_unary_expression(lexer);
		return exp;
	case T_COLON:
	case T_LBRACE:
		unlex(lexer, &tok);
		return parse_compound_expression(lexer);
	case T_MATCH:
		return parse_match_expression(lexer);
	case T_SWITCH:
		return parse_switch_expression(lexer);
	default:
		unlex(lexer, &tok);
		return parse_builtin_expression(lexer);
	}
}

static struct ast_expression *
parse_cast_expression(struct lexer *lexer, struct ast_expression *value)
{
	if (value == NULL) {
		value = parse_unary_expression(lexer);
	}
	enum cast_kind kind;
	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_COLON:
		kind = C_CAST;
		break;
	case T_AS:
		kind = C_ASSERTION;
		break;
	case T_IS:
		kind = C_TEST;
		break;
	default:
		unlex(lexer, &tok);
		return value;
	}

	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_CAST;
	exp->cast.kind = kind;
	exp->cast.value = value;
	if (kind == C_CAST) {
		exp->cast.type = parse_type(lexer);
	} else {
		if (lex(lexer, &tok) == T_NULL) {
			exp->cast.type = mktype(tok.loc);
			exp->cast.type->storage = STORAGE_NULL;
		} else {
			unlex(lexer, &tok);
			exp->cast.type = parse_type(lexer);
		}
	}
	return parse_cast_expression(lexer, exp);
}

static int
precedence(enum lexical_token token)
{
	switch (token) {
	case T_LOR:
		return 0;
	case T_LXOR:
		return 1;
	case T_LAND:
		return 2;
	case T_LEQUAL:
	case T_NEQUAL:
		return 3;
	case T_LESS:
	case T_LESSEQ:
	case T_GREATER:
	case T_GREATEREQ:
		return 4;
	case T_BOR:
		return 5;
	case T_BXOR:
		return 6;
	case T_BAND:
		return 7;
	case T_LSHIFT:
	case T_RSHIFT:
		return 8;
	case T_PLUS:
	case T_MINUS:
		return 9;
	case T_TIMES:
	case T_DIV:
	case T_MODULO:
		return 10;
	default:
		return -1;
	}
	assert(0); // Unreachable
}

static enum binarithm_operator
binop_for_token(enum lexical_token tok)
{
	switch (tok) {
	case T_LOR:
		return BIN_LOR;
	case T_LAND:
		return BIN_LAND;
	case T_LXOR:
		return BIN_LXOR;
	case T_BOR:
		return BIN_BOR;
	case T_BXOR:
		return BIN_BXOR;
	case T_BAND:
		return BIN_BAND;
	case T_LEQUAL:
		return BIN_LEQUAL;
	case T_NEQUAL:
		return BIN_NEQUAL;
	case T_LESS:
		return BIN_LESS;
	case T_LESSEQ:
		return BIN_LESSEQ;
	case T_GREATER:
		return BIN_GREATER;
	case T_GREATEREQ:
		return BIN_GREATEREQ;
	case T_LSHIFT:
		return BIN_LSHIFT;
	case T_RSHIFT:
		return BIN_RSHIFT;
	case T_PLUS:
		return BIN_PLUS;
	case T_MINUS:
		return BIN_MINUS;
	case T_TIMES:
		return BIN_TIMES;
	case T_DIV:
		return BIN_DIV;
	case T_MODULO:
		return BIN_MODULO;
	default:
		assert(0); // Invariant
	}
	assert(0); // Unreachable
}

static struct ast_expression *
parse_bin_expression(struct lexer *lexer, struct ast_expression *lvalue, int i)
{
	struct token tok;
	lex(lexer, &tok);

	int j;
	while ((j = precedence(tok.token)) >= i) {
		enum binarithm_operator op = binop_for_token(tok.token);

		struct ast_expression *rvalue =
			parse_cast_expression(lexer, NULL);
		lex(lexer, &tok);

		int k;
		while ((k = precedence(tok.token)) > j) {
			unlex(lexer, &tok);
			rvalue = parse_bin_expression(lexer, rvalue, k);
			lex(lexer, &tok);
		}

		struct ast_expression *e = mkexpr(lexer->loc);
		e->type = EXPR_BINARITHM;
		e->binarithm.op = op;
		e->binarithm.lvalue = lvalue;
		e->binarithm.rvalue = rvalue;
		lvalue = e;
	}

	unlex(lexer, &tok);
	return lvalue;
}

static struct ast_expression *
parse_if_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_IF;

	struct token tok = {0};

	want(lexer, T_LPAREN, &tok);
	exp->_if.cond = parse_expression(lexer);
	want(lexer, T_RPAREN, &tok);

	exp->_if.true_branch = parse_expression(lexer);

	switch (lex(lexer, &tok)) {
	case T_ELSE:
		if (lex(lexer, &tok) == T_IF) {
			exp->_if.false_branch = parse_if_expression(lexer);
		} else {
			unlex(lexer, &tok);
			exp->_if.false_branch = parse_expression(lexer);
		}
		break;
	default:
		unlex(lexer, &tok);
		break;
	}
	return exp;
}

static void parse_for_predicate(struct lexer *lexer,
	struct ast_expression_for *for_exp)
{
	struct token tok = {0};
	unsigned int flags = 0;

	switch (lex(lexer, &tok)) {
	case T_CONST:
		flags = TYPE_CONST;
		break;
	case T_LET:
		break;
	default:
		unlex(lexer, &tok);
		for_exp->kind = FOR_ACCUMULATOR;
		for_exp->cond = parse_expression(lexer);
		break;
	}
	if (for_exp->cond == NULL) {
		for_exp->bindings = mkexpr(lexer->loc);
		for_exp->bindings->type = EXPR_BINDING;

		struct ast_expression_binding *binding = &for_exp->bindings->binding;

		bool for_kind_found = false;
		while (true) {
			binding->flags = flags;

			switch (lex(lexer, &tok)) {
			case T_NAME:
				binding->name = tok.name;
				break;
			case T_LPAREN:
				parse_binding_unpack(lexer, &binding->unpack);
				break;
			default:
				synerr(&tok, T_NAME, T_LPAREN, T_EOF);
			}

			if (lex(lexer, &tok) == T_COLON) {
				binding->type = parse_type(lexer);
				binding->type->flags |= flags;
			} else {
				unlex(lexer, &tok);
			}

			if (for_kind_found) {
				want(lexer, T_EQUAL, &tok);
			} else {
				for_kind_found = true;

				switch (lex(lexer, &tok)) {
				case T_DOUBLE_DOT:
					for_exp->kind = FOR_EACH_VALUE;
					break;
				case T_BAND:
					want(lexer, T_DOUBLE_DOT, &tok);
					for_exp->kind = FOR_EACH_POINTER;
					break;
				case T_ARROW:
					for_exp->kind = FOR_EACH_ITERATOR;
					break;
				case T_EQUAL:
					for_exp->kind = FOR_ACCUMULATOR;
					break;
				default:
					synerr(&tok, T_DOUBLE_DOT, T_BAND,
						T_ARROW, T_EQUAL, T_EOF);
				}
			}
			binding->initializer = parse_expression(lexer);

			if (for_exp->kind != FOR_ACCUMULATOR) {
				return;
			}
			if (lex(lexer, &tok) != T_COMMA) {
				unlex(lexer, &tok);
				break;
			}
			binding->next = xcalloc(1, sizeof(struct ast_expression_binding));
			binding = binding->next;
		}
		want(lexer, T_SEMICOLON, &tok);

		for_exp->cond = parse_expression(lexer);
	}

	if (lex(lexer, &tok) != T_SEMICOLON) {
		unlex(lexer, &tok);
		return;
	}

	for_exp->afterthought = parse_expression(lexer);
}

static struct ast_expression *
parse_for_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_FOR;

	struct token tok = {0};
	want(lexer, T_FOR, &tok);

	switch (lex(lexer, &tok)) {
	case T_COLON:
		want(lexer, T_NAME, &tok);
		exp->_for.label = tok.name;
		want(lexer, T_LPAREN, &tok);
		break;
	case T_LPAREN:
		break;
	default:
		synerr(&tok, T_LPAREN, T_COLON, T_EOF);
		break;
	}

	parse_for_predicate(lexer, &exp->_for);

	want(lexer, T_RPAREN, &tok);

	exp->_for.body = parse_expression(lexer);
	return exp;
}

static struct ast_case_option *
parse_case_options(struct lexer *lexer)
{
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_ARROW:
		return NULL; // Default case
	default:
		unlex(lexer, &tok);
		break;
	}

	bool more = true;
	struct ast_case_option *opt = xcalloc(1, sizeof(struct ast_case_option));
	struct ast_case_option *opts = opt;
	struct ast_case_option **next = &opt->next;
	while (more) {
		opt->value = parse_expression(lexer);
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			switch (lex(lexer, &tok)) {
			case T_ARROW:
				more = false;
				break;
			default:
				unlex(lexer, &tok);
				opt = xcalloc(1, sizeof(struct ast_case_option));
				*next = opt;
				next = &opt->next;
				break;
			}
			break;
		case T_ARROW:
			more = false;
			break;
		default:
			synerr(&tok, T_COMMA, T_ARROW, T_EOF);
			break;
		}
	}

	return opts;
}

static struct ast_expression *
parse_switch_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_SWITCH;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_COLON:
		want(lexer, T_NAME, &tok);
		exp->_switch.label = tok.name;
		want(lexer, T_LPAREN, &tok);
		break;
	case T_LPAREN:
		break; // no-op
	default:
		synerr(&tok, T_LPAREN, T_COLON, T_EOF);
	}
	exp->_switch.value = parse_expression(lexer);
	want(lexer, T_RPAREN, &tok);
	want(lexer, T_LBRACE, &tok);

	bool more = true;
	struct ast_switch_case **next_case = &exp->_switch.cases;
	while (more) {
		struct ast_switch_case *_case =
			*next_case = xcalloc(1, sizeof(struct ast_switch_case));
		want(lexer, T_CASE, &tok);
		_case->options = parse_case_options(lexer);

		bool exprs = true;
		struct ast_expression_list *cur = &_case->exprs;
		struct ast_expression_list **next = &cur->next;
		while (exprs) {
			cur->expr = parse_statement(lexer);
			want(lexer, T_SEMICOLON, &tok);

			switch (lex(lexer, &tok)) {
			case T_CASE:
			case T_RBRACE:
				exprs = false;
				break;
			default:
				break;
			}
			unlex(lexer, &tok);

			if (exprs) {
				*next = xcalloc(1, sizeof(struct ast_expression_list));
				cur = *next;
				next = &cur->next;
			}
		}

		switch (lex(lexer, &tok)) {
		case T_CASE:
			unlex(lexer, &tok);
			break;
		case T_RBRACE:
			more = false;
			break;
		default:
			synerr(&tok, T_CASE, T_RBRACE, T_EOF);
		}

		next_case = &_case->next;
	}

	return exp;
}

static struct ast_expression *
parse_match_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_MATCH;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_COLON:
		want(lexer, T_NAME, &tok);
		exp->match.label = tok.name;
		want(lexer, T_LPAREN, &tok);
		break;
	case T_LPAREN:
		break; // no-op
	default:
		synerr(&tok, T_LPAREN, T_COLON, T_EOF);
	}
	exp->match.value = parse_expression(lexer);
	want(lexer, T_RPAREN, &tok);
	want(lexer, T_LBRACE, &tok);

	bool more = true;
	struct ast_match_case **next_case = &exp->match.cases;
	while (more) {
		struct ast_match_case *_case =
			*next_case = xcalloc(1, sizeof(struct ast_match_case));
		want(lexer, T_CASE, &tok);

		struct ast_type *type = NULL;
		switch (lex(lexer, &tok)) {
		case T_LET:
			want(lexer, T_NAME, &tok);
			_case->name = tok.name;
			want(lexer, T_COLON, NULL);
			_case->type = parse_type(lexer);
			break;
		case T_ARROW:
			// Default case
			unlex(lexer, &tok);
			break;
		case T_NULL:
			type = mktype(tok.loc);
			type->storage = STORAGE_NULL;
			_case->type = type;
			break;
		default:
			unlex(lexer, &tok);
			_case->type = parse_type(lexer);
			break;
		}

		want(lexer, T_ARROW, &tok);

		bool exprs = true;
		struct ast_expression_list *cur = &_case->exprs;
		struct ast_expression_list **next = &cur->next;
		while (exprs) {
			cur->expr = parse_statement(lexer);
			want(lexer, T_SEMICOLON, &tok);

			switch (lex(lexer, &tok)) {
			case T_CASE:
			case T_RBRACE:
				exprs = false;
				break;
			default:
				break;
			}
			unlex(lexer, &tok);

			if (exprs) {
				*next = xcalloc(1, sizeof(struct ast_expression_list));
				cur = *next;
				next = &cur->next;
			}
		}

		switch (lex(lexer, &tok)) {
		case T_CASE:
			unlex(lexer, &tok);
			break;
		case T_RBRACE:
			more = false;
			break;
		default:
			synerr(&tok, T_CASE, T_RBRACE, T_EOF);
		}

		next_case = &_case->next;
	}
	return exp;
}

static void
parse_binding_unpack(struct lexer *lexer, struct ast_binding_unpack **next)
{
	struct token tok = {0};

	bool more = true;
	while (more) {
		char *name = NULL;
		switch (lex(lexer, &tok)) {
		case T_NAME:
			name = tok.name;
			break;
		case T_UNDERSCORE:
			break;
		default:
			synerr(&tok, T_NAME, T_UNDERSCORE, T_EOF);
		}

		struct ast_binding_unpack *new = xcalloc(1, sizeof *new);
		*next = new;
		next = &new->next;

		new->name = name;

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			break;
		case T_RPAREN:
			more = false;
			break;
		default:
			synerr(&tok, T_COMMA, T_RPAREN, T_EOF);
		}
	}
}

static struct ast_expression *
parse_binding_list(struct lexer *lexer, bool is_static)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	unsigned int flags = 0;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_DEF:
		exp->type = EXPR_DEFINE;
		break;
	case T_CONST:
		flags = TYPE_CONST;
		// fallthrough
	case T_LET:
		exp->type = EXPR_BINDING;
		break;
	default:
		synerr(&tok, T_LET, T_CONST, T_DEF, T_EOF);
	}

	struct ast_expression_binding *binding = &exp->binding;
	struct ast_expression_binding **next = &exp->binding.next;

	bool more = true;
	while (more) {
		switch (lex(lexer, &tok)) {
		case T_NAME:
			binding->name = tok.name;
			break;
		case T_LPAREN:
			if (exp->type == EXPR_BINDING) {
				parse_binding_unpack(lexer, &binding->unpack);
				break;
			}
			// fallthrough
		default:
			synerr(&tok, T_NAME, T_LPAREN, T_EOF);
		}
		binding->initializer = mkexpr(lexer->loc);
		binding->flags = flags;
		binding->is_static = is_static;

		switch (lex(lexer, &tok)) {
		case T_COLON:
			binding->type = parse_type(lexer);
			binding->type->flags |= flags;
			want(lexer, T_EQUAL, &tok);
			break;
		case T_EQUAL:
			break;
		default:
			synerr(&tok, T_COLON, T_EQUAL, T_EOF);
		}

		binding->initializer = parse_expression(lexer);

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			*next = xcalloc(1, sizeof(struct ast_expression_binding));
			binding = *next;
			next = &binding->next;
			break;
		default:
			unlex(lexer, &tok);
			more = false;
			break;
		}
	}
	return exp;
}

static struct ast_expression *
parse_assignment(struct lexer *lexer, struct ast_expression *object,
	enum binarithm_operator op)
{
	struct ast_expression *value = parse_expression(lexer);
	struct ast_expression *expr = mkexpr(lexer->loc);
	expr->type = EXPR_ASSIGN;
	expr->assign.op = op;
	expr->assign.object = object;
	expr->assign.value = value;
	return expr;
}

static struct ast_expression *
parse_deferred_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_DEFER;
	exp->defer.deferred = parse_expression(lexer);
	return exp;
}

static struct ast_expression *
parse_control_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);

	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_BREAK:
	case T_CONTINUE:
		exp->type = tok.token == T_BREAK ? EXPR_BREAK : EXPR_CONTINUE;
		exp->control.label = NULL;
		switch (lex(lexer, &tok)) {
		case T_COLON:
			want(lexer, T_NAME, &tok);
			exp->control.label = tok.name;
			break;
		default:
			unlex(lexer, &tok);
			break;
		}
		break;
	case T_RETURN:
		exp->type = EXPR_RETURN;
		exp->_return.value = NULL;
		switch (lex(lexer, &tok)) {
		case T_COMMA:
		case T_ELSE:
		case T_RBRACE:
		case T_RBRACKET:
		case T_RPAREN:
		case T_SEMICOLON:
			unlex(lexer, &tok);
			break;
		default:
			unlex(lexer, &tok);
			exp->_return.value = parse_expression(lexer);
			break;
		}
		break;
	case T_YIELD:
		exp->type = EXPR_YIELD;
		exp->control.value = NULL;
		switch (lex(lexer, &tok)) {
		case T_COMMA:
		case T_ELSE:
		case T_RBRACE:
		case T_RBRACKET:
		case T_RPAREN:
		case T_SEMICOLON:
			unlex(lexer, &tok);
			break;
		case T_COLON:
			want(lexer, T_NAME, &tok);
			exp->control.label = tok.name;
			switch (lex(lexer, &tok)) {
			case T_COMMA:
				exp->control.value = parse_expression(lexer);
				break;
			default:
				unlex(lexer, &tok);
				break;
			}
			break;
		default:
			unlex(lexer, &tok);
			exp->control.value = parse_expression(lexer);
			break;
		}
		break;
	default:
		synerr(&tok,
			T_BREAK, T_CONTINUE, T_RETURN, T_YIELD, T_EOF);
	}
	return exp;
}

static struct ast_expression *
parse_compound_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(lexer->loc);
	exp->type = EXPR_COMPOUND;

	struct ast_expression_list *cur = &exp->compound.list;
	struct ast_expression_list **next = &cur->next;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_COLON:
		want(lexer, T_NAME, &tok);
		exp->compound.label = tok.name;
		want(lexer, T_LBRACE, &tok);
		break;
	case T_LBRACE:
		break; // no-op
	default:
		synerr(&tok, T_LBRACE, T_COLON, T_EOF);
		break;
	}

	while (true) {
		cur->expr = parse_statement(lexer);

		want(lexer, T_SEMICOLON, &tok);

		lex(lexer, &tok);
		if (tok.token == T_RBRACE) {
			break;
		} 

		unlex(lexer, &tok);
		*next = xcalloc(1, sizeof(struct ast_expression_list));
		cur = *next;
		next = &cur->next;
	}
	
	return exp;
}

struct ast_expression *
parse_expression(struct lexer *lexer)
{
	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_STATIC:
		return parse_static_expression(lexer, false);
	case T_BREAK:
	case T_CONTINUE:
	case T_RETURN:
	case T_YIELD:
	case T_FOR:
	case T_IF:
		switch (tok.token) {
		case T_BREAK:
		case T_CONTINUE:
		case T_RETURN:
		case T_YIELD:
			unlex(lexer, &tok);
			return parse_control_expression(lexer);
		case T_FOR:
			unlex(lexer, &tok);
			return parse_for_expression(lexer);
		case T_IF:
			return parse_if_expression(lexer);
		default:
			assert(0);
		}
	default:
		break;
	}

	unlex(lexer, &tok);
	struct ast_expression *value = parse_unary_expression(lexer);
	if (value->type != EXPR_ACCESS && value->type != EXPR_SLICE
			&& (value->type != EXPR_UNARITHM
				|| value->unarithm.op != UN_DEREF)) {
		value = parse_cast_expression(lexer, value);
		return parse_bin_expression(lexer, value, 0);
	}

	// Is object-selector, try for assignment
	switch (lex(lexer, &tok)) {
	case T_EQUAL:
		return parse_assignment(lexer, value, BIN_LEQUAL);
	case T_BANDEQ:
		return parse_assignment(lexer, value, BIN_BAND);
	case T_LANDEQ:
		return parse_assignment(lexer, value, BIN_LAND);
	case T_DIVEQ:
		return parse_assignment(lexer, value, BIN_DIV);
	case T_LSHIFTEQ:
		return parse_assignment(lexer, value, BIN_LSHIFT);
	case T_MINUSEQ:
		return parse_assignment(lexer, value, BIN_MINUS);
	case T_MODEQ:
		return parse_assignment(lexer, value, BIN_MODULO);
	case T_BOREQ:
		return parse_assignment(lexer, value, BIN_BOR);
	case T_LOREQ:
		return parse_assignment(lexer, value, BIN_LOR);
	case T_PLUSEQ:
		return parse_assignment(lexer, value, BIN_PLUS);
	case T_RSHIFTEQ:
		return parse_assignment(lexer, value, BIN_RSHIFT);
	case T_TIMESEQ:
		return parse_assignment(lexer, value, BIN_TIMES);
	case T_BXOREQ:
		return parse_assignment(lexer, value, BIN_BXOR);
	case T_LXOREQ:
		return parse_assignment(lexer, value, BIN_LXOR);
	default:
		unlex(lexer, &tok);
		value = parse_cast_expression(lexer, value);
		value = parse_bin_expression(lexer, value, 0);
		return value;
	}
}

static struct ast_expression *
parse_statement(struct lexer *lexer)
{
	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_LET:
	case T_CONST:
	case T_DEF:
		unlex(lexer, &tok);
		return parse_binding_list(lexer, false);
	case T_STATIC:
		return parse_static_expression(lexer, true);
	case T_DEFER:
		return parse_deferred_expression(lexer);
	default:
		unlex(lexer, &tok);
		return parse_expression(lexer);
	}
}

static char *
parse_attr_symbol(struct lexer *lexer)
{
	struct token tok = {0};
	want(lexer, T_LPAREN, NULL);
	want(lexer, T_NUMBER, &tok);
	synassert_msg(tok.storage == STORAGE_STRING,
		"expected string literal", &tok);
	synassert_msg(tok.string.len > 0, "invalid symbol", &tok);
	for (size_t i = 0; i < tok.string.len; i++) {
		uint32_t c = tok.string.value[i];
		synassert_msg(c <= 0x7F && (isalnum(c) || c == '_' || c == '$'
			|| c == '.'), "invalid symbol", &tok);
		synassert_msg(i != 0 || (!isdigit(c) && c != '$'),
			"invalid symbol", &tok);
	}
	want(lexer, T_RPAREN, NULL);
	return tok.string.value;
}

static void
parse_global_decl(struct lexer *lexer, enum lexical_token mode,
		struct ast_global_decl *decl)
{
	struct token tok = {0};
	struct ast_global_decl *i = decl;
	assert(mode == T_LET || mode == T_CONST || mode == T_DEF);
	bool more = true;
	while (more) {
		if (mode == T_LET || mode == T_CONST) {
			switch (lex(lexer, &tok)) {
			case T_ATTR_SYMBOL:
				i->symbol = parse_attr_symbol(lexer);
				break;
			default:
				unlex(lexer, &tok);
				break;
			}
			switch (lex(lexer, &tok)) {
			case T_ATTR_THREADLOCAL:
				i->threadlocal = true;
				break;
			default:
				unlex(lexer, &tok);
				break;
			}
		}
		parse_identifier(lexer, &i->ident, false);
		switch (lex(lexer, &tok)) {
		case T_COLON:
			i->type = parse_type(lexer);
			if (mode == T_CONST) {
				i->type->flags |= TYPE_CONST;
			}
			if (lex(lexer, &tok) != T_EQUAL) {
				synassert(mode != T_DEF, &tok, T_EQUAL, T_EOF);
				unlex(lexer, &tok);
				break;
			}
			/* fallthrough */
		case T_EQUAL:
			i->init = parse_expression(lexer);
			break;
		default:
			synerr(&tok, T_EQUAL, T_COLON, T_EOF);
		}

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			lex(lexer, &tok);
			if (tok.token == T_NAME
					|| tok.token == T_ATTR_SYMBOL) {
				i->next = xcalloc(1, sizeof(struct ast_global_decl));
				i = i->next;
				unlex(lexer, &tok);
				break;
			}
			/* fallthrough */
		default:
			more = false;
			unlex(lexer, &tok);
			break;
		}
	}
}

static void
parse_type_decl(struct lexer *lexer, struct ast_type_decl *decl)
{
	struct token tok = {0};
	struct ast_type_decl *i = decl;
	bool more = true;
	while (more) {
		parse_identifier(lexer, &i->ident, false);
		want(lexer, T_EQUAL, NULL);
		switch (lex(lexer, &tok)) {
		case T_ENUM:
			i->type = parse_enum_type(&i->ident, lexer);
			break;
		default:
			unlex(lexer, &tok);
			i->type = parse_type(lexer);
		}
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			if (lex(lexer, &tok) == T_NAME) {
				i->next = xcalloc(1, sizeof(struct ast_type_decl));
				i = i->next;
				unlex(lexer, &tok);
				break;
			}
			/* fallthrough */
		default:
			more = false;
			unlex(lexer, &tok);
			break;
		}
	}
}

static void
parse_fn_decl(struct lexer *lexer, struct ast_function_decl *decl)
{
	struct token tok = {0};
	bool more = true;
	while (more) {
		switch (lex(lexer, &tok)) {
		case T_ATTR_FINI:
			decl->flags |= FN_FINI;
			break;
		case T_ATTR_INIT:
			decl->flags |= FN_INIT;
			break;
		case T_ATTR_SYMBOL:
			decl->symbol = parse_attr_symbol(lexer);
			break;
		case T_ATTR_TEST:
			decl->flags |= FN_TEST;
			break;
		default:
			more = false;
			unlex(lexer, &tok);
			break;
		}
	}
	want(lexer, T_FN, NULL);
	parse_identifier(lexer, &decl->ident, false);
	parse_prototype(lexer, &decl->prototype);

	switch (lex(lexer, &tok)) {
	case T_EQUAL:
		decl->body = parse_expression(lexer);
		break;
	case T_SEMICOLON:
		unlex(lexer, &tok);
		decl->body = NULL; // Prototype
		break;
	default:
		synerr(&tok, T_EQUAL, T_SEMICOLON, T_EOF);
	}
}

static void
parse_decl(struct lexer *lexer, struct ast_decl *decl)
{
	struct token tok = {0};
	decl->loc = lexer->loc;
	switch (lex(lexer, &tok)) {
	case T_CONST:
	case T_LET:
		decl->decl_type = ADECL_GLOBAL;
		parse_global_decl(lexer, tok.token, &decl->global);
		break;
	case T_DEF:
		decl->decl_type = ADECL_CONST;
		parse_global_decl(lexer, tok.token, &decl->constant);
		break;
	case T_TYPE:
		decl->decl_type = ADECL_TYPE;
		parse_type_decl(lexer, &decl->type);
		break;
	default:
		unlex(lexer, &tok);
		decl->decl_type = ADECL_FUNC;
		parse_fn_decl(lexer, &decl->function);
		break;
	}
}

static void
parse_decls(struct lexer *lexer, struct ast_decls **decls)
{
	struct token tok = {0};
	struct ast_decls **next = decls;
	while (tok.token != T_EOF) {
		struct ast_decls *decl = *next =
			xcalloc(1, sizeof(struct ast_decls));
		switch (lex(lexer, &tok)) {
		case T_EXPORT:
			decl->decl.exported = true;
			break;
		case T_STATIC:
			decl->decl.decl_type = ADECL_ASSERT;
			parse_assertion(lexer, true, &decl->decl.assert);
			next = &decl->next;
			want(lexer, T_SEMICOLON, NULL);
			continue;
		default:
			unlex(lexer, &tok);
			break;
		}
		if (tok.token == T_EOF) {
			break;
		}
		parse_decl(lexer, &decl->decl);
		next = &decl->next;
		want(lexer, T_SEMICOLON, NULL);
	}
	free(*next);
	*next = 0;
}

void
parse(struct lexer *lexer, struct ast_subunit *subunit)
{
	parse_imports(lexer, subunit);
	parse_decls(lexer, &subunit->decls);
	want(lexer, T_EOF, NULL);
}
