#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "check.h"
#include "identifier.h"
#include "lex.h"
#include "parse.h"
#include "types.h"
#include "utf8.h"
#include "util.h"

static void
synassert_msg(bool cond, const char *msg, struct token *tok)
{
	if (!cond) {
		fprintf(stderr, "Syntax error: %s at %s:%d:%d (found '%s')\n", msg,
			tok->loc.path, tok->loc.lineno, tok->loc.colno,
			token_str(tok));
		exit(EXIT_FAILURE);
	}
}

static void
synassert(bool cond, struct token *tok, ...)
{
	if (!cond) {
		va_list ap;
		va_start(ap, tok);

		enum lexical_token t = va_arg(ap, enum lexical_token);
		fprintf(stderr,
			"Syntax error: unexpected '%s' at %s:%d:%d%s",
			token_str(tok), tok->loc.path, tok->loc.lineno,
			tok->loc.colno, t == T_EOF ? "\n" : ", expected " );
		while (t != T_EOF) {
			if (t == T_LITERAL || t == T_NAME) {
				fprintf(stderr, "%s", lexical_token_str(t));
			} else {
				fprintf(stderr, "'%s'", lexical_token_str(t));
			}
			t = va_arg(ap, enum lexical_token);
			fprintf(stderr, "%s", t == T_EOF ? "\n" : ", ");
		}
		exit(EXIT_FAILURE);
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
static struct location
locdup(const struct location *loc)
{
	struct location new_loc = {
		.lineno = loc->lineno,
		.colno = loc->colno,
		.path = strdup(loc->path),
	};
	return new_loc;
}

static struct ast_expression *
mkexpr(const struct location *loc)
{
	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	exp->loc.lineno = loc->lineno;
	exp->loc.colno = loc->colno;
	exp->loc.path = strdup(loc->path);
	return exp;
}

static struct ast_type *
mktype(const struct location *loc)
{
	struct ast_type *t = xcalloc(1, sizeof(struct ast_type));
	t->loc.lineno = loc->lineno;
	t->loc.colno = loc->colno;
	t->loc.path = strdup(loc->path);
	return t;
}

static struct ast_function_parameters *
mkfuncparams(const struct location *loc)
{
	struct ast_function_parameters *p =
		xcalloc(1, sizeof(struct ast_function_parameters));
	p->loc.lineno = loc->lineno;
	p->loc.colno = loc->colno;
	p->loc.path = strdup(loc->path);
	return p;
}

bool
parse_identifier(struct lexer *lexer, struct identifier *ident, bool trailing)
{
	struct token tok = {0};
	struct identifier *i = ident;
	bool found_trailing = false;
	while (!i->name) {
		switch (lex(lexer, &tok)) {
		case T_NAME:
			i->name = strdup(tok.name);
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
	return found_trailing;
}

static void
parse_name_list(struct lexer *lexer, struct ast_imports *name)
{
	bool more = true;
	struct ast_imports **next = &name->next;
	while (more) {
		struct token tok = {0};
		want(lexer, T_NAME, &tok);
		name->ident.name = strdup(tok.name);
		name->loc = locdup(&tok.loc);
		token_finish(&tok);

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			switch (lex(lexer, &tok)) {
			case T_RBRACE:
				more = false;
				break;
			default:
				unlex(lexer, &tok);
				name = xcalloc(1, sizeof(struct ast_imports));
				*next = name;
				next = &name->next;
			}
			break;
		case T_RBRACE:
			more = false;
			break;
		default:
			synassert(false, &tok, T_RBRACE, T_COMMA, T_EOF);
			break;
		}
	}
}

static void
parse_import(struct lexer *lexer, struct ast_imports *imports)
{
	struct identifier ident = {0};
	bool trailing_colon = parse_identifier(lexer, &ident, true);

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_EQUAL:
		synassert(!trailing_colon, &tok, T_NAME, T_EOF);
		imports->mode = AST_IMPORT_ALIAS;
		imports->alias = xcalloc(1, sizeof(struct identifier));
		*imports->alias = ident;
		parse_identifier(lexer, &imports->ident, false);
		want(lexer, T_SEMICOLON, &tok);
		break;
	case T_LBRACE:
		synassert(trailing_colon, &tok, T_DOUBLE_COLON, T_EOF);
		imports->mode = AST_IMPORT_MEMBERS;
		imports->ident = ident;
		imports->members = xcalloc(1, sizeof(struct ast_imports));
		parse_name_list(lexer, imports->members);
		want(lexer, T_SEMICOLON, &tok);
		break;
	case T_SEMICOLON:
		synassert(!trailing_colon, &tok, T_NAME, T_EOF);
		imports->mode = AST_IMPORT_IDENTIFIER;
		imports->ident = ident;
		break;
	default:
		synassert(!trailing_colon, &tok, T_EQUAL, T_SEMICOLON, T_EOF);
		synassert(trailing_colon, &tok, T_NAME, T_LBRACE, T_EOF);
		break;
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
	struct token tok = {0};
	bool more = true;
	type->params = mkfuncparams(&lexer->loc);
	struct ast_function_parameters *next = type->params;
	while (more) {
		switch (lex(lexer, &tok)) {
		case T_UNDERSCORE:
			break;
		case T_NAME:
			next->name = tok.name; // Assumes ownership
			break;
		default:
			synassert(false, &tok, T_UNDERSCORE, T_NAME, T_EOF);
		}
		want(lexer, T_COLON, NULL);
		next->type = parse_type(lexer);

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			switch (lex(lexer, &tok)) {
			case T_ELLIPSIS:
				type->variadism = VARIADISM_C;
				if (lex(lexer, &tok) != T_COMMA) {
					unlex(lexer, &tok);
				}
				more = false;
				break;
			case T_RPAREN:
				more = false;
				unlex(lexer, &tok);
				break;
			default:
				unlex(lexer, &tok);
				next->next = mkfuncparams(&lexer->loc);
				next = next->next;
				break;
			}
			break;
		case T_ELLIPSIS:
			type->variadism = VARIADISM_HARE;
			if (lex(lexer, &tok) != T_COMMA) {
				unlex(lexer, &tok);
			}
			more = false;
			break;
		default:
			more = false;
			unlex(lexer, &tok);
			break;
		}
	}
}

static void
parse_prototype(struct lexer *lexer, struct ast_function_type *type)
{
	want(lexer, T_LPAREN, NULL);
	struct token tok = {0};
	if (lex(lexer, &tok) != T_RPAREN) {
		unlex(lexer, &tok);
		parse_parameter_list(lexer, type);
		want(lexer, T_RPAREN, NULL);
	}
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
	case T_CHAR:
		return STORAGE_CHAR;
	default:
		assert(0);
	}
}

static struct ast_type *
parse_primitive_type(struct lexer *lexer)
{
	struct token tok = {0};
	struct ast_type *type = mktype(&lexer->loc);
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
	case T_CHAR:
		unlex(lexer, &tok);
		type->storage = parse_integer_type(lexer);
		break;
	case T_RUNE:
		type->storage = STORAGE_RUNE;
		break;
	case T_STR:
		type->storage = STORAGE_STRING;
		break;
	case T_TYPE:
		type->storage = STORAGE_TYPE;
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
	default:
		assert(0);
	}
	return type;
}

static struct ast_expression *parse_binding_list(
		struct lexer *lexer, bool is_static);
static struct ast_expression *parse_object_selector(struct lexer *lexer);

static struct ast_type *
parse_enum_type(struct lexer *lexer)
{
	struct token tok = {0};
	struct ast_type *type = mktype(&lexer->loc);
	type->storage = STORAGE_ENUM;
	struct ast_enum_field **next = &type->_enum.values;
	switch (lex(lexer, &tok)) {
	case T_LBRACE:
		type->_enum.storage = STORAGE_INT;
		unlex(lexer, &tok);
		break;
	default:
		unlex(lexer, &tok);
		type->_enum.storage = parse_integer_type(lexer);
		break;
	}
	want(lexer, T_LBRACE, NULL);
	while (tok.token != T_RBRACE) {
		*next = xcalloc(1, sizeof(struct ast_enum_field));
		want(lexer, T_NAME, &tok);
		(*next)->name = tok.name;
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
			synassert(false, &tok, T_COMMA, T_RBRACE, T_EOF);
		}
	}
	return type;
}

static struct ast_type *
parse_struct_union_type(struct lexer *lexer)
{
	struct token tok = {0};
	struct ast_type *type = mktype(&lexer->loc);
	struct ast_struct_union_type *next = &type->struct_union;
	switch (lex(lexer, &tok)) {
	case T_STRUCT:
		type->storage = STORAGE_STRUCT;
		break;
	case T_UNION:
		type->storage = STORAGE_UNION;
		break;
	default:
		synassert(false, &tok, T_STRUCT, T_UNION, T_EOF);
		break;
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
				next->type = mktype(&loc);
				next->type->storage = STORAGE_ALIAS;
				next->type->unwrap = false;
				parse_identifier(lexer, &next->type->alias, false);
				struct identifier *i = &next->type->alias;
				while (i->ns != NULL) {
					i = i->ns;
				}
				i->ns = xcalloc(sizeof(struct identifier), 1);
				i->ns->name = name;
				break;
			default:
				unlex(lexer, &tok);
				next->type = mktype(&loc);
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
			synassert(false, &tok, T_NAME, T_STRUCT, T_UNION, T_EOF);
		}
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			if (lex(lexer, &tok) != T_RBRACE) {
				unlex(lexer, &tok);
				next->next = xcalloc(1,
					sizeof(struct ast_struct_union_type));
				next = next->next;
			}
			break;
		case T_RBRACE:
			break;
		default:
			synassert(false, &tok, T_COMMA, T_RBRACE, T_EOF);
		}
	}
	return type;
}

static struct ast_type *
parse_tagged_type(struct lexer *lexer, struct ast_type *first)
{
	struct ast_type *type = mktype(&first->loc);
	type->storage = STORAGE_TAGGED;
	struct ast_tagged_union_type *next = &type->tagged_union;
	next->type = first;
	struct token tok = {0};
	while (tok.token != T_RPAREN) {
		next->next = xcalloc(sizeof(struct ast_tagged_union_type), 1);
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
			synassert(false, &tok, T_BOR, T_RPAREN, T_EOF);
		}
	}
	return type;
}

static struct ast_type *
parse_tuple_type(struct lexer *lexer, struct ast_type *first)
{
	struct ast_type *type = mktype(&first->loc);
	type->storage = STORAGE_TUPLE;
	struct ast_tuple_type *next = &type->tuple;
	next->type = first;
	struct token tok = {0};
	while (tok.token != T_RPAREN) {
		next->next = xcalloc(sizeof(struct ast_tuple_type), 1);
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
			synassert(false, &tok, T_COMMA, T_RPAREN, T_EOF);
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
		synassert(false, &tok, T_BOR, T_COMMA, T_EOF);
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
	bool noreturn = false, nullable = false, unwrap = false;
	switch (lex(lexer, &tok)) {
	case T_BOOL:
	case T_CHAR:
	case T_F32:
	case T_F64:
	case T_I16:
	case T_I32:
	case T_I64:
	case T_I8:
	case T_INT:
	case T_RUNE:
	case T_SIZE:
	case T_STR:
	case T_TYPE:
	case T_U16:
	case T_U32:
	case T_U64:
	case T_U8:
	case T_UINT:
	case T_UINTPTR:
	case T_VOID:
		unlex(lexer, &tok);
		type = parse_primitive_type(lexer);
		break;
	case T_ENUM:
		type = parse_enum_type(lexer);
		break;
	case T_NULLABLE:
		nullable = true;
		want(lexer, T_TIMES, NULL);
		/* fallthrough */
	case T_TIMES:
		type = mktype(&lexer->loc);
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
		type = mktype(&lexer->loc);
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
	case T_ATTR_NORETURN:
		noreturn = true;
		want(lexer, T_FN, NULL);
		// fallthrough
	case T_FN:
		type = mktype(&lexer->loc);
		type->storage = STORAGE_FUNCTION;
		parse_prototype(lexer, &type->func);
		if (noreturn) {
			type->func.flags |= FN_NORETURN;
		}
		break;
	case T_ELLIPSIS:
		unwrap = true;
		want(lexer, T_NAME, &tok);
		// Fallthrough
	case T_NAME:
		unlex(lexer, &tok);
		type = mktype(&lexer->loc);
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
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_ACCESS;
	exp->access.type = ACCESS_IDENTIFIER;
	exp->access.ident = ident;
	return exp;
}

static struct ast_expression *
parse_constant(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_CONSTANT;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_TRUE:
		exp->constant.storage = STORAGE_BOOL;
		exp->constant.bval = true;
		return exp;
	case T_FALSE:
		exp->constant.storage = STORAGE_BOOL;
		exp->constant.bval = false;
		return exp;
	case T_NULL:
		exp->constant.storage = STORAGE_NULL;
		return exp;
	case T_VOID:
		exp->constant.storage = STORAGE_VOID;
		return exp;
	case T_LITERAL:
		exp->constant.storage = tok.storage;
		break;
	default:
		synassert(false, &tok, T_LITERAL, T_TRUE,
			T_FALSE, T_NULL, T_VOID, T_EOF);
		break;
	}

	switch (tok.storage) {
	case STORAGE_CHAR:
	case STORAGE_U8:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_SIZE:
		exp->constant.uval = (uintmax_t)tok.uval;
		break;
	case STORAGE_I8:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_ICONST:
	case STORAGE_INT:
		exp->constant.ival = (intmax_t)tok.ival;
		break;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
		exp->constant.fval = tok.fval;
		break;
	case STORAGE_RUNE:
		exp->constant.rune = tok.rune;
		break;
	case STORAGE_STRING:
		exp->constant.string.len = tok.string.len;
		exp->constant.string.value = tok.string.value;
		while (lex(lexer, &tok) == T_LITERAL
				&& tok.storage == STORAGE_STRING) {
			size_t len = exp->constant.string.len;
			exp->constant.string.value = xrealloc(
				exp->constant.string.value,
				len + tok.string.len);
			memcpy(exp->constant.string.value + len,
				tok.string.value, tok.string.len);
			exp->constant.string.len += tok.string.len;
		}
		unlex(lexer, &tok);
		break;
	case STORAGE_BOOL:
	case STORAGE_NULL:
	case STORAGE_VOID:
		assert(0); // Handled above
	case STORAGE_ALIAS:
	case STORAGE_ARRAY:
	case STORAGE_ENUM:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_SLICE:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_TYPE:
		assert(0); // Handled in a different nonterminal
	}
	return exp;
}

static struct ast_expression *
parse_array_literal(struct lexer *lexer)
{
	struct token tok;
	want(lexer, T_LBRACKET, &tok);

	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_CONSTANT;
	exp->constant.storage = STORAGE_ARRAY;

	struct ast_array_constant *item, **next = &exp->constant.array;

	while (lex(lexer, &tok) != T_RBRACKET) {
		unlex(lexer, &tok);

		item = *next = xcalloc(1, sizeof(struct ast_array_constant));
		item->value = parse_expression(lexer);
		next = &item->next;

		switch (lex(lexer, &tok)) {
		case T_ELLIPSIS:
			item->expand = true;
			lex(lexer, &tok);
			if (tok.token == T_COMMA) {
				want(lexer, T_RBRACKET, &tok);
				unlex(lexer, &tok);
			} else if (tok.token == T_RBRACKET) {
				unlex(lexer, &tok);
			} else {
				synassert(false, &tok, T_COMMA, T_RBRACKET, T_EOF);
			}
			break;
		case T_COMMA:
			// Move on
			break;
		case T_RBRACKET:
			unlex(lexer, &tok);
			break;
		default:
			synassert(false, &tok, T_ELLIPSIS, T_COMMA, T_RBRACKET, T_EOF);
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
		xcalloc(sizeof(struct ast_field_value), 1);
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
			i->ns = xcalloc(sizeof(struct identifier), 1);
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
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_STRUCT;
	exp->_struct.type = ident;
	struct ast_field_value **next = &exp->_struct.fields;
	struct token tok = {0};
	while (tok.token != T_RBRACE) {
		switch (lex(lexer, &tok)) {
		case T_ELLIPSIS:
			synassert(ident.name != NULL, &tok, T_RBRACE, T_EOF);
			exp->_struct.autofill = true;
			if (lex(lexer, &tok) != T_COMMA) {
				unlex(lexer, &tok);
			}
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
			synassert(false, &tok, T_ELLIPSIS, T_NAME, T_RBRACE,
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
			synassert(false, &tok, T_COMMA, T_RBRACE, T_EOF);
		}
	}
	return exp;
}

static struct ast_expression *
parse_tuple_expression(struct lexer *lexer, struct ast_expression *first)
{
	struct ast_expression *exp = mkexpr(&first->loc);
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
			synassert(false, &tok, T_RPAREN, T_COMMA, T_EOF);
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
	case T_LITERAL:
	case T_TRUE:
	case T_FALSE:
	case T_NULL:
	case T_VOID:
		unlex(lexer, &tok);
		return parse_constant(lexer);
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
			synassert(false, &tok, T_RPAREN, T_COMMA, T_EOF);
		};
		assert(0); // Unreachable
	default:
		synassert(false, &tok, T_LITERAL, T_NAME,
			T_LBRACKET, T_STRUCT, T_LPAREN, T_EOF);
	}
	assert(0); // Unreachable
}

static struct ast_expression *
parse_assertion_expression(struct lexer *lexer, bool is_static)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_ASSERT;
	exp->assert.is_static = is_static;

	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_ASSERT:
	case T_ABORT:
		break;
	default:
		synassert(false, &tok, T_ASSERT, T_ABORT, T_EOF);
	}

	switch (tok.token) {
	case T_ASSERT:
		want(lexer, T_LPAREN, &tok);
		exp->assert.cond = parse_expression(lexer);
		if (lex(lexer, &tok) == T_COMMA) {
			exp->assert.message = parse_constant(lexer);
		} else {
			unlex(lexer, &tok);
		}
		want(lexer, T_RPAREN, &tok);
		break;
	case T_ABORT:
		want(lexer, T_LPAREN, &tok);
		if (lex(lexer, &tok) != T_RPAREN) {
			unlex(lexer, &tok);
			exp->assert.message = parse_constant(lexer);
			want(lexer, T_RPAREN, &tok);
		}
		break;
	default:
		assert(0); // Invariant
	}

	return exp;
}

static struct ast_expression *
parse_measurement_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_MEASURE;

	struct token tok;
	lex(lexer, &tok);

	want(lexer, T_LPAREN, NULL);
	switch (tok.token) {
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
		synassert(false, &tok, T_SIZE, T_LEN, T_OFFSET, T_EOF);
	}

	want(lexer, T_RPAREN, NULL);
	return exp;
}

static struct ast_expression *
parse_type_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_TYPE;
	struct token tok;
	lex(lexer, &tok);

	want(lexer, T_LPAREN, NULL);
	exp->_type.type = parse_type(lexer);
	want(lexer, T_RPAREN, NULL);
	return exp;
}

static struct ast_expression *
parse_call_expression(struct lexer *lexer, struct ast_expression *lvalue)
{
	struct token tok;
	want(lexer, T_LPAREN, &tok);

	struct ast_expression *expr = mkexpr(&lexer->loc);
	expr->type = EXPR_CALL;
	expr->call.lvalue = lvalue;

	struct ast_call_argument *arg, **next = &expr->call.args;
	while (lex(lexer, &tok) != T_RPAREN) {
		unlex(lexer, &tok);
		arg = *next = xcalloc(1, sizeof(struct ast_call_argument));
		arg->value = parse_expression(lexer);

		if (lex(lexer, &tok) == T_ELLIPSIS) {
			arg->variadic = true;
		} else {
			unlex(lexer, &tok);
		}

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			break;
		case T_RPAREN:
			unlex(lexer, &tok);
			break;
		default:
			synassert(false, &tok, T_COMMA, T_RPAREN, T_EOF);
		}

		next = &arg->next;
	}
	return expr;
}

static struct ast_expression *
parse_index_slice_expression(struct lexer *lexer, struct ast_expression *lvalue)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	struct ast_expression *start = NULL, *end = NULL;
	struct token tok;
	want(lexer, T_LBRACKET, &tok);

	bool is_slice = false;
	switch (lex(lexer, &tok)) {
	case T_SLICE:
		is_slice = true;
		break;
	default:
		unlex(lexer, &tok);
		start = parse_expression(lexer);
		break;
	}

	switch (lex(lexer, &tok)) {
	case T_SLICE:
		is_slice = true;
		break;
	case T_RBRACKET:
		break;
	default:
		if (is_slice) {
			unlex(lexer, &tok);
			break;
		}
		synassert(false, &tok, T_SLICE, T_RBRACKET, T_EOF);
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
		exp = mkexpr(&tok.loc);
		exp->type = EXPR_ALLOC;
		want(lexer, T_LPAREN, NULL);
		exp->alloc.expr = parse_expression(lexer);
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			exp->alloc.cap = parse_expression(lexer);
			want(lexer, T_RPAREN, NULL);
			break;
		case T_RPAREN:
			break;
		default:
			synassert(false, &tok, T_COMMA, T_RPAREN, T_EOF);
		}
		break;
	case T_FREE:
		exp = mkexpr(&tok.loc);
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
parse_slice_mutation(struct lexer *lexer, bool is_static)
{
	struct ast_expression *exp = NULL;
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_APPEND:
	case T_INSERT:
		exp = mkexpr(&tok.loc);
		exp->type = tok.token == T_APPEND ? EXPR_APPEND : EXPR_INSERT;
		want(lexer, T_LPAREN, NULL);

		struct ast_append_values **next;
		switch (exp->type) {
		case EXPR_APPEND:
			exp->append.expr = parse_object_selector(lexer);
			exp->append.is_static = is_static;
			next = &exp->append.values;
			break;
		case EXPR_INSERT:
			exp->insert.expr = parse_object_selector(lexer);
			exp->insert.is_static = is_static;
			next = &exp->insert.values;
			break;
		default:
			assert(0);
		}

		want(lexer, T_COMMA, NULL);

		while (tok.token != T_RPAREN) {
			if (lex(lexer, &tok) == T_ELLIPSIS) {
				exp->append.variadic =
					parse_expression(lexer);
				break;
			}
			*next = xcalloc(1, sizeof(struct ast_append_values));
			unlex(lexer, &tok);
			(*next)->expr = parse_expression(lexer);

			lex(lexer, &tok);
			if (tok.token == T_ELLIPSIS) {
				exp->append.variadic = (*next)->expr;
				free(*next);
				*next = NULL;
				if (lex(lexer, &tok) != T_COMMA) {
					unlex(lexer, &tok);
				}
				want(lexer, T_RPAREN, &tok);
				break;
			} else if (tok.token != T_COMMA) {
				unlex(lexer, &tok);
				want(lexer, T_RPAREN, &tok);
				break;
			}

			next = &(*next)->next;
		}
		break;
	case T_DELETE:
		exp = mkexpr(&tok.loc);
		exp->type = EXPR_DELETE;
		want(lexer, T_LPAREN, NULL);
		exp->delete.expr = parse_expression(lexer);
		exp->delete.is_static = is_static;
		want(lexer, T_RPAREN, NULL);
		break;
	default:
		assert(0);
	}
	return exp;
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
		exp = mkexpr(&lexer->loc);
		exp->type = EXPR_ACCESS;

		switch (lex(lexer, &tok)) {
		case T_NAME:
			exp->access.type = ACCESS_FIELD;
			exp->access._struct = lvalue;
			exp->access.field = tok.name;
			break;
		case T_LITERAL:
			exp->access.type = ACCESS_TUPLE;
			exp->access.tuple = lvalue;
			unlex(lexer, &tok);
			exp->access.value = parse_constant(lexer);
			break;
		default:
			synassert(false, &tok, T_NAME, T_LITERAL, T_EOF);
		}

		lvalue = exp;
		break;
	case T_LBRACKET:
		unlex(lexer, &tok);
		lvalue = parse_index_slice_expression(lexer, lvalue);
		break;
	case T_QUESTION:
	case T_LNOT:
		exp = mkexpr(&lexer->loc);
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
	case T_PLUS:	// +
		return UN_PLUS;
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
	synassert_msg(exp->type == EXPR_ACCESS, "expected object", &tok);
	return exp;
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
		switch (lex(lexer, &tok)) {
		case T_ABORT:
		case T_ASSERT:
			unlex(lexer, &tok);
			return parse_assertion_expression(lexer, true);
		case T_APPEND:
		case T_DELETE:
		case T_INSERT:
			unlex(lexer, &tok);
			return parse_slice_mutation(lexer, true);
		default:
			synassert(false, &tok, T_ABORT, T_ASSERT,
					T_APPEND, T_DELETE, T_INSERT, T_EOF);
		};
		break;
	case T_ABORT:
	case T_ASSERT:
		unlex(lexer, &tok);
		return parse_assertion_expression(lexer, false);
	case T_SIZE:
	case T_LEN:
	case T_OFFSET:
		unlex(lexer, &tok);
		return parse_measurement_expression(lexer);
	case T_TYPE:
		unlex(lexer, &tok);
		return parse_type_expression(lexer);
	default:
		unlex(lexer, &tok);
		break;
	}
	return parse_postfix_expression(lexer, NULL);
}

static struct ast_expression *
parse_unary_expression(struct lexer *lexer)
{
	struct token tok;
	struct ast_expression *exp;
	switch (lex(lexer, &tok)) {
	case T_PLUS:	// +
	case T_MINUS:	// -
	case T_BNOT:	// ~
	case T_LNOT:	// !
	case T_TIMES:	// *
	case T_BAND:	// &
		exp = mkexpr(&lexer->loc);
		exp->type = EXPR_UNARITHM;
		exp->unarithm.op = unop_for_token(tok.token);
		exp->unarithm.operand = parse_unary_expression(lexer);
		return exp;
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

	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_CAST;
	exp->cast.kind = kind;
	exp->cast.value = value;
	exp->cast.type = parse_type(lexer);
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
	if (!lvalue) {
		lvalue = parse_cast_expression(lexer, NULL);
	}

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

		struct ast_expression *e = mkexpr(&lexer->loc);
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
	struct ast_expression *exp = mkexpr(&lexer->loc);
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

static struct ast_expression *
parse_for_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_FOR;

	struct token tok = {0};
	want(lexer, T_FOR, &tok);
	want(lexer, T_LPAREN, &tok);
	switch (lex(lexer, &tok)) {
	case T_LET:
	case T_CONST:
		unlex(lexer, &tok);
		exp->_for.bindings = parse_binding_list(lexer, false);
		want(lexer, T_SEMICOLON, &tok);
		break;
	default:
		unlex(lexer, &tok);
		break;
	}

	exp->_for.cond = parse_expression(lexer);

	switch (lex(lexer, &tok)) {
	case T_SEMICOLON:
		exp->_for.afterthought = parse_expression(lexer);
		want(lexer, T_RPAREN, &tok);
		break;
	case T_RPAREN:
		break;
	default:
		synassert(false, &tok, T_SEMICOLON, T_RPAREN, T_EOF);
	}

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
			synassert(false, &tok, T_COMMA, T_ARROW, T_EOF);
			break;
		}
	}

	return opts;
}

static struct ast_expression *
parse_switch_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_SWITCH;

	struct token tok = {0};
	want(lexer, T_LPAREN, &tok);
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
			cur->expr = parse_expression(lexer);
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
			synassert(false, &tok, T_CASE, T_RBRACE, T_EOF);
		}

		next_case = &_case->next;
	}

	return exp;
}

static struct ast_expression *
parse_match_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_MATCH;

	struct token tok = {0};
	want(lexer, T_LPAREN, &tok);
	exp->match.value = parse_expression(lexer);
	want(lexer, T_RPAREN, &tok);
	want(lexer, T_LBRACE, &tok);

	bool more = true;
	struct ast_match_case **next_case = &exp->match.cases;
	while (more) {
		struct ast_match_case *_case =
			*next_case = xcalloc(1, sizeof(struct ast_match_case));
		want(lexer, T_CASE, &tok);

		struct token tok2 = {0};
		struct identifier ident = {0};
		struct ast_type *type = NULL;
		switch (lex(lexer, &tok)) {
		case T_NAME:
			switch (lex(lexer, &tok2)) {
			case T_COLON:
				_case->name = tok.name; // Assumes ownership
				_case->type = parse_type(lexer);
				break;
			case T_DOUBLE_COLON:
				ident.ns = xcalloc(1, sizeof(struct identifier));
				ident.ns->name = tok.name; // Assumes ownership
				parse_identifier(lexer, &ident, false);
				_case->type = mktype(&tok.loc);
				_case->type->storage = STORAGE_ALIAS;
				_case->type->alias = ident;
				break;
			case T_ARROW:
				unlex(lexer, &tok2);
				_case->type = mktype(&tok.loc);
				_case->type->storage = STORAGE_ALIAS;
				_case->type->alias.name = tok.name;
				break;
			default:
				synassert(false, &tok, T_COLON,
					T_DOUBLE_COLON, T_ARROW, T_EOF);
				break;
			}
			break;
		case T_ARROW:
			// Default case
			unlex(lexer, &tok);
			break;
		case T_NULL:
			type = mktype(&tok.loc);
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
			cur->expr = parse_expression(lexer);
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
			synassert(false, &tok, T_CASE, T_RBRACE, T_EOF);
		}

		next_case = &_case->next;
	}
	return exp;
}

static struct ast_expression *
parse_binding_list(struct lexer *lexer, bool is_static)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_BINDING;
	unsigned int flags = 0;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_CONST:
		flags = TYPE_CONST;
		// fallthrough
	case T_LET:
		// no-op
		break;
	default:
		synassert(false, &tok, T_LET, T_CONST, T_EOF);
	}

	struct ast_expression_binding *binding = &exp->binding;
	struct ast_expression_binding **next = &exp->binding.next;

	bool more = true;
	while (more) {
		want(lexer, T_NAME, &tok);
		binding->name = tok.name;
		binding->initializer = mkexpr(&lexer->loc);
		binding->flags = flags;
		binding->is_static = is_static;

		switch (lex(lexer, &tok)) {
		case T_COLON:
			binding->type = parse_type(lexer);
			binding->type->flags |= flags;
			want(lexer, T_EQUAL, &tok);
			binding->initializer = parse_expression(lexer);
			break;
		case T_EQUAL:
			binding->initializer = parse_expression(lexer);
			break;
		default:
			synassert(false, &tok, T_COLON, T_EQUAL, T_EOF);
		}

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
	bool indirect, enum binarithm_operator op)
{
	struct ast_expression *value = parse_expression(lexer);
	struct ast_expression *expr = mkexpr(&lexer->loc);
	expr->type = EXPR_ASSIGN;
	expr->assign.op = op;
	expr->assign.object = object;
	expr->assign.value = value;
	expr->assign.indirect = indirect;
	return expr;
}

static struct ast_expression *
parse_deferred_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_DEFER;
	exp->defer.deferred = parse_expression(lexer);
	return exp;
}

static struct ast_expression *
parse_control_statement(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);

	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_BREAK:
	case T_CONTINUE:
		exp->type = tok.token == T_BREAK ? EXPR_BREAK : EXPR_CONTINUE;
		exp->control.label = NULL;
		switch (lex(lexer, &tok)) {
		case T_LABEL:
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
		case T_SEMICOLON:
		case T_COMMA:
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
		case T_SEMICOLON:
			unlex(lexer, &tok);
			break;
		case T_LABEL:
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
		synassert(false, &tok,
			T_BREAK, T_CONTINUE, T_RETURN, T_YIELD, T_EOF);
	}
	return exp;
}

static struct ast_expression *
parse_compound_expression(struct lexer *lexer)
{
	struct ast_expression *exp = mkexpr(&lexer->loc);
	exp->type = EXPR_COMPOUND;

	struct ast_expression_list *cur = &exp->compound.list;
	struct ast_expression_list **next = &cur->next;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_LABEL:
		exp->compound.label = tok.name;
		want(lexer, T_LBRACE, &tok);
		break;
	case T_LBRACE:
		break; // no-op
	default:
		synassert(false, &tok, T_LBRACE, T_LABEL, T_EOF);
		break;
	};

	bool more = true;
	while (more) {
		cur->expr = parse_expression(lexer);

		want(lexer, T_SEMICOLON, &tok);

		if (more) {
			lex(lexer, &tok);
			if (tok.token == T_RBRACE) {
				more = false;
			} else {
				unlex(lexer, &tok);
				*next = xcalloc(1, sizeof(struct ast_expression_list));
				cur = *next;
				next = &cur->next;
			}
		} else {
			want(lexer, T_RBRACE, &tok);
		}
	}
	return exp;
}

struct ast_expression *
parse_expression(struct lexer *lexer)
{
	// This is one of the more complicated non-terminals to parse.
	struct token tok;
	bool indirect = false;
	switch (lex(lexer, &tok)) {
	case T_TIMES: // *ptr = value (or unary-expression)
		indirect = true;
		break;
	default:
		unlex(lexer, &tok);
		break;
	}

	struct ast_expression *value;
	switch (lex(lexer, &tok)) {
	case T_LET:
	case T_CONST:
		unlex(lexer, &tok);
		return parse_binding_list(lexer, false);
	case T_STATIC:
		switch (lex(lexer, &tok)) {
		case T_LET:
		case T_CONST:
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
			synassert(false, &tok, T_LET, T_CONST, T_ABORT, T_ASSERT, T_EOF);
		}
		assert(0); // Unreachable
	case T_BREAK:
	case T_CONTINUE:
	case T_RETURN:
	case T_YIELD:
	case T_DEFER:
	case T_FOR:
	case T_LABEL:
	case T_IF:
	case T_LBRACE:
	case T_MATCH:
	case T_SWITCH:
		switch (tok.token) {
		case T_BREAK:
		case T_CONTINUE:
		case T_RETURN:
		case T_YIELD:
			unlex(lexer, &tok);
			value = parse_control_statement(lexer);
			break;
		case T_DEFER:
			value = parse_deferred_expression(lexer);
			break;
		case T_FOR:
			unlex(lexer, &tok);
			value = parse_for_expression(lexer);
			break;
		case T_IF:
			value = parse_if_expression(lexer);
			break;
		case T_LBRACE:
		case T_LABEL:
			unlex(lexer, &tok);
			value = parse_compound_expression(lexer);
			break;
		case T_MATCH:
			value = parse_match_expression(lexer);
			break;
		case T_SWITCH:
			value = parse_switch_expression(lexer);
			break;
		default:
			assert(0);
		}
		if (indirect) {
			struct ast_expression *deref = mkexpr(&value->loc);
			deref->type = EXPR_UNARITHM;
			deref->unarithm.op = UN_DEREF;
			deref->unarithm.operand = value;
			return deref;
		}
		return value;
	default:
		unlex(lexer, &tok);
		value = parse_unary_expression(lexer);
		if (!indirect && value->type != EXPR_ACCESS
				&& value->type != EXPR_SLICE) {
			value = parse_cast_expression(lexer, value);
			return parse_bin_expression(lexer, value, 0);
		}
		// Is possible object-selector, try for assignment
		break;
	}

	switch (lex(lexer, &tok)) {
	case T_EQUAL:
		return parse_assignment(lexer, value, indirect, BIN_LEQUAL);
	case T_BANDEQ:
		return parse_assignment(lexer, value, indirect, BIN_BAND);
	case T_LANDEQ:
		return parse_assignment(lexer, value, indirect, BIN_LAND);
	case T_DIVEQ:
		return parse_assignment(lexer, value, indirect, BIN_DIV);
	case T_LSHIFTEQ:
		return parse_assignment(lexer, value, indirect, BIN_LSHIFT);
	case T_MINUSEQ:
		return parse_assignment(lexer, value, indirect, BIN_MINUS);
	case T_MODEQ:
		return parse_assignment(lexer, value, indirect, BIN_MODULO);
	case T_BOREQ:
		return parse_assignment(lexer, value, indirect, BIN_BOR);
	case T_LOREQ:
		return parse_assignment(lexer, value, indirect, BIN_LOR);
	case T_PLUSEQ:
		return parse_assignment(lexer, value, indirect, BIN_PLUS);
	case T_RSHIFTEQ:
		return parse_assignment(lexer, value, indirect, BIN_RSHIFT);
	case T_TIMESEQ:
		return parse_assignment(lexer, value, indirect, BIN_TIMES);
	case T_BXOREQ:
		return parse_assignment(lexer, value, indirect, BIN_BXOR);
	case T_LXOREQ:
		return parse_assignment(lexer, value, indirect, BIN_LXOR);
	default:
		unlex(lexer, &tok);
		if (indirect) {
			struct ast_expression *deref = mkexpr(&value->loc);
			deref->type = EXPR_UNARITHM;
			deref->unarithm.op = UN_DEREF;
			deref->unarithm.operand = value;
			value = deref;
		}
		value = parse_cast_expression(lexer, value);
		value = parse_bin_expression(lexer, value, 0);
		return value;
	}
}

static char *
parse_attr_symbol(struct lexer *lexer)
{
	struct token tok = {0};
	want(lexer, T_LPAREN, NULL);
	want(lexer, T_LITERAL, &tok);
	synassert_msg(tok.storage == STORAGE_STRING,
		"expected string literal", &tok);
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
			case T_ATTR_HIDDEN:
				i->hidden = true;
				break;
			default:
				unlex(lexer, &tok);
				break;
			}
		}
		parse_identifier(lexer, &i->ident, false);
		want(lexer, T_COLON, NULL);
		i->type = parse_type(lexer);
		if (mode == T_CONST) {
			i->type->flags |= TYPE_CONST;
		}

		if (lex(lexer, &tok) == T_EQUAL) {
			i->init = parse_expression(lexer);
		} else {
			unlex(lexer, &tok);
		}

		switch (lex(lexer, &tok)) {
		case T_COMMA:
			lex(lexer, &tok);
			if (tok.token == T_NAME
					|| tok.token == T_ATTR_SYMBOL
					|| tok.token == T_ATTR_HIDDEN) {
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
		i->type = parse_type(lexer);
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
	bool noreturn = false;
	while (more) {
		switch (lex(lexer, &tok)) {
		case T_ATTR_FINI:
			decl->flags |= FN_FINI;
			break;
		case T_ATTR_HIDDEN:
			decl->hidden = true;
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
		case T_ATTR_NORETURN:
			noreturn = true;
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
	if (noreturn) {
		decl->prototype.flags |= FN_NORETURN;
	}

	switch (lex(lexer, &tok)) {
	case T_EQUAL:
		decl->body = parse_expression(lexer);
		break;
	case T_SEMICOLON:
		unlex(lexer, &tok);
		decl->body = NULL; // Prototype
		break;
	default:
		synassert(false, &tok, T_EQUAL, T_SEMICOLON, T_EOF);
	}
}

static void
parse_decl(struct lexer *lexer, struct ast_decl *decl)
{
	struct token tok = {0};
	decl->loc.lineno = lexer->loc.lineno;
	decl->loc.colno = lexer->loc.colno;
	decl->loc.path = strdup(lexer->loc.path);
	switch (lex(lexer, &tok)) {
	case T_CONST:
	case T_LET:
		decl->decl_type = AST_DECL_GLOBAL;
		parse_global_decl(lexer, tok.token, &decl->global);
		break;
	case T_DEF:
		decl->decl_type = AST_DECL_CONST;
		parse_global_decl(lexer, tok.token, &decl->constant);
		break;
	case T_TYPE:
		decl->decl_type = AST_DECL_TYPE;
		parse_type_decl(lexer, &decl->type);
		break;
	default:
		unlex(lexer, &tok);
		decl->decl_type = AST_DECL_FUNC;
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
		if (lex(lexer, &tok) != T_EOF) {
			unlex(lexer, &tok);
		}
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
