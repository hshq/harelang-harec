#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "identifier.h"
#include "lex.h"
#include "parse.h"
#include "trace.h"
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
		exit(1);
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
		exit(1);
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

void
parse_identifier(struct lexer *lexer, struct identifier *ident)
{
	struct token tok = {0};
	struct identifier *i = ident;
	trenter(TR_PARSE, "identifier");

	while (!i->name) {
		want(lexer, T_NAME, &tok);
		i->name = strdup(tok.name);
		token_finish(&tok);

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

	char buf[1024];
	identifier_unparse_static(ident, buf, sizeof(buf));
	trleave(TR_PARSE, "%s", buf);
}

static void
parse_import(struct lexer *lexer, struct ast_imports *imports)
{
	trenter(TR_PARSE, "import");
	struct identifier ident = {0};
	parse_identifier(lexer, &ident);

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_EQUAL:
		assert(0); // TODO
	case T_LBRACE:
		assert(0); // TODO
	case T_SEMICOLON:
		imports->mode = AST_IMPORT_IDENTIFIER;
		imports->ident = ident;
		break;
	default:
		synassert(false, &tok, T_EQUAL, T_LBRACE, T_SEMICOLON, T_EOF);
		break;
	}

	trleave(TR_PARSE, NULL);
}

static void
parse_imports(struct lexer *lexer, struct ast_subunit *subunit)
{
	trenter(TR_PARSE, "imports");
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

	for (struct ast_imports *i = subunit->imports; i; i = i->next) {
		char buf[1024];
		identifier_unparse_static(&i->ident, buf, sizeof(buf));
		trace(TR_PARSE, "use %s", buf);
	}
	trleave(TR_PARSE, NULL);
}

static struct ast_type *parse_type(struct lexer *lexer);

static void
parse_parameter_list(struct lexer *lexer, struct ast_function_type *type)
{
	trenter(TR_PARSE, "parameter-list");
	struct token tok = {0};
	bool more = true;
	type->params = xcalloc(sizeof(struct ast_function_parameters), 1);
	struct ast_function_parameters *next = type->params;
	while (more) {
		want(lexer, T_NAME, &tok);
		next->name = strdup(tok.name);
		token_finish(&tok);
		want(lexer, T_COLON, NULL);
		next->type = parse_type(lexer);
		trace(TR_PARSE, "%s: [type]", next->name);
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			switch (lex(lexer, &tok)) {
			case T_ELLIPSIS:
				type->variadism = VARIADISM_HARE;
				if (lex(lexer, &tok) != T_COMMA) {
					unlex(lexer, &tok);
				}
				more = false;
				trace(TR_PARSE, ", ...");
				break;
			default:
				unlex(lexer, &tok);
				next->next =
					xcalloc(sizeof(struct ast_function_parameters), 1);
				next = next->next;
				break;
			}
			break;
		case T_ELLIPSIS:
			type->variadism = VARIADISM_C;
			if (lex(lexer, &tok) != T_COMMA) {
				unlex(lexer, &tok);
			}
			more = false;
			trace(TR_PARSE, "...");
			break;
		default:
			more = false;
			unlex(lexer, &tok);
			break;
		}
	}
	trleave(TR_PARSE, NULL);
}

static void
parse_prototype(struct lexer *lexer, struct ast_function_type *type)
{
	trenter(TR_PARSE, "prototype");
	want(lexer, T_LPAREN, NULL);
	struct token tok = {0};
	if (lex(lexer, &tok) != T_RPAREN) {
		unlex(lexer, &tok);
		parse_parameter_list(lexer, type);
		want(lexer, T_RPAREN, NULL);
	}
	type->result = parse_type(lexer);
	size_t ctr = 0;
	for (struct ast_function_parameters *param = type->params;
			param; param = param->next) {
		ctr++;
	}
	trace(TR_PARSE, "[%zu parameters] [type]", ctr);
	trleave(TR_PARSE, NULL);
}

static enum type_storage
parse_integer_type(struct lexer *lexer)
{
	trenter(TR_PARSE, "integer");
	enum type_storage storage;
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_I8:
		storage = TYPE_STORAGE_I8;
		break;
	case T_I16:
		storage = TYPE_STORAGE_I16;
		break;
	case T_I32:
		storage = TYPE_STORAGE_I32;
		break;
	case T_I64:
		storage = TYPE_STORAGE_I64;
		break;
	case T_U8:
		storage = TYPE_STORAGE_U8;
		break;
	case T_U16:
		storage = TYPE_STORAGE_U16;
		break;
	case T_U32:
		storage = TYPE_STORAGE_U32;
		break;
	case T_U64:
		storage = TYPE_STORAGE_U64;
		break;
	case T_INT:
		storage = TYPE_STORAGE_INT;
		break;
	case T_UINT:
		storage = TYPE_STORAGE_UINT;
		break;
	case T_SIZE:
		storage = TYPE_STORAGE_SIZE;
		break;
	case T_UINTPTR:
		storage = TYPE_STORAGE_UINTPTR;
		break;
	case T_CHAR:
		storage = TYPE_STORAGE_CHAR;
		break;
	default:
		assert(0);
	}
	trleave(TR_PARSE, "%s", type_storage_unparse(storage));
	return storage;
}

static struct ast_type *
parse_primitive_type(struct lexer *lexer)
{
	trenter(TR_PARSE, "primitive");
	struct token tok = {0};
	struct ast_type *type = xcalloc(sizeof(struct ast_type), 1);
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
		type->storage = TYPE_STORAGE_RUNE;
		break;
	case T_STR:
		type->storage = TYPE_STORAGE_STRING;
		break;
	case T_F32:
		type->storage = TYPE_STORAGE_F32;
		break;
	case T_F64:
		type->storage = TYPE_STORAGE_F64;
		break;
	case T_BOOL:
		type->storage = TYPE_STORAGE_BOOL;
		break;
	case T_VOID:
		type->storage = TYPE_STORAGE_VOID;
		break;
	default:
		assert(0);
	}
	trleave(TR_PARSE, "%s", type_storage_unparse(type->storage));
	return type;
}

static struct ast_expression *parse_simple_expression(struct lexer *lexer);
static struct ast_expression *parse_complex_expression(struct lexer *lexer);
static struct ast_expression *parse_compound_expression(struct lexer *lexer);
static struct ast_expression *parse_scope_expression(struct lexer *lexer);
static struct ast_expression *parse_binding_list(struct lexer *lexer);

static struct ast_type *
parse_enum_type(struct lexer *lexer)
{
	trenter(TR_PARSE, "enum");
	struct token tok = {0};
	struct ast_type *type = xcalloc(sizeof(struct ast_type), 1);
	type->storage = TYPE_STORAGE_ENUM;
	struct ast_enum_field **next = &type->_enum.values;
	switch (lex(lexer, &tok)) {
	case T_LBRACE:
		type->_enum.storage = TYPE_STORAGE_INT;
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
			(*next)->value = parse_simple_expression(lexer);
			trace(TR_PARSE, "%s = [expr]", (*next)->name);
		} else {
			unlex(lexer, &tok);
			trace(TR_PARSE, "%s = [generated]", (*next)->name);
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
	trleave(TR_PARSE, NULL);
	return type;
}

static struct ast_type *
parse_struct_union_type(struct lexer *lexer)
{
	struct token tok = {0};
	struct ast_type *type = xcalloc(sizeof(struct ast_type), 1);
	struct ast_struct_union_type *next;
	switch (lex(lexer, &tok)) {
	case T_STRUCT:
		trenter(TR_PARSE, "struct");
		type->storage = TYPE_STORAGE_STRUCT;
		next = &type->_struct;
		break;
	case T_UNION:
		trenter(TR_PARSE, "union");
		type->storage = TYPE_STORAGE_UNION;
		next = &type->_union;
		break;
	default:
		synassert(false, &tok, T_STRUCT, T_UNION, T_EOF);
		break;
	}
	want(lexer, T_LBRACE, NULL);
	while (tok.token != T_RBRACE) {
		char *name;
		switch (lex(lexer, &tok)) {
		case T_NAME:
			name = tok.name;
			struct identifier *i;
			switch (lex(lexer, &tok)) {
			case T_COLON:
				next->member_type = MEMBER_TYPE_FIELD;
				next->field.name = name;
				next->field.type = parse_type(lexer);
				trace(TR_PARSE, "%s: [type]", name);
				break;
			case T_DOUBLE_COLON:
				next->member_type = MEMBER_TYPE_ALIAS;
				i = &next->alias;
				parse_identifier(lexer, i);
				while (i->ns != NULL) {
					i = i->ns;
				}
				i->ns = xcalloc(sizeof(struct identifier), 1);
				i->ns->name = name;
				trace(TR_PARSE, "[embedded alias %s]",
					identifier_unparse(&next->alias));
				break;
			default:
				unlex(lexer, &tok);
				next->member_type = MEMBER_TYPE_ALIAS;
				next->alias.name = name;
				trace(TR_PARSE, "[embedded alias %s]", name);
				break;
			}
			break;
		case T_STRUCT:
		case T_UNION:
			unlex(lexer, &tok);
			next->embedded = parse_struct_union_type(lexer);
			trace(TR_PARSE, "[embedded struct/union]");
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
	trleave(TR_PARSE, NULL);
	return type;
}

static struct ast_type *
parse_tagged_union_type(struct lexer *lexer)
{
	trenter(TR_PARSE, "tagged union");
	struct ast_type *type = xcalloc(sizeof(struct ast_type), 1);
	type->storage = TYPE_STORAGE_TAGGED_UNION;
	struct ast_tagged_union_type *next = &type->tagged_union;
	next->type = parse_type(lexer);
	struct token tok = {0};
	want(lexer, T_BOR, &tok);
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
	trleave(TR_PARSE, NULL);
	return type;
}

static struct ast_type *
parse_type(struct lexer *lexer)
{
	trenter(TR_PARSE, "type");
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
	struct ast_type *type = NULL;
	bool noreturn = false;
	bool nullable = false;
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
	case T_RUNE:
	case T_STR:
	case T_F32:
	case T_F64:
	case T_BOOL:
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
		trace(TR_PARSE, "nullable");
		/* fallthrough */
	case T_TIMES:
		type = xcalloc(sizeof(struct ast_type), 1);
		type->storage = TYPE_STORAGE_POINTER;
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
		type = parse_tagged_union_type(lexer);
		break;
	case T_LBRACKET:
		type = xcalloc(sizeof(struct ast_type), 1);
		switch (lex(lexer, &tok)) {
		case T_RBRACKET:
			type->storage = TYPE_STORAGE_SLICE;
			type->slice.members = parse_type(lexer);
			break;
		default:
			type->storage = TYPE_STORAGE_ARRAY;
			unlex(lexer, &tok);
			type->array.length = parse_simple_expression(lexer);
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
		type = xcalloc(sizeof(struct ast_type), 1);
		type->storage = TYPE_STORAGE_FUNCTION;
		parse_prototype(lexer, &type->func);
		if (noreturn) {
			type->func.flags |= FN_NORETURN;
		}
		break;
	case T_NAME:
		unlex(lexer, &tok);
		type = xcalloc(sizeof(struct ast_type), 1);
		type->storage = TYPE_STORAGE_ALIAS;
		parse_identifier(lexer, &type->alias);
		break;
	default:
		synassert_msg(false, "expected type", &tok);
		break;
	}
	type->flags |= flags;
	trleave(TR_PARSE, "%s%s", type->flags & TYPE_CONST ? "const " : "",
		type_storage_unparse(type->storage));
	return type;
}

static struct ast_expression *
parse_access(struct lexer *lexer, struct identifier ident)
{
	trace(TR_PARSE, "access");
	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_ACCESS;
	exp->access.type = ACCESS_IDENTIFIER;
	exp->access.ident = ident;
	return exp;
}

static struct ast_expression *
parse_constant(struct lexer *lexer)
{
	trenter(TR_PARSE, "constant");

	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_CONSTANT;

	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_TRUE:
		exp->constant.storage = TYPE_STORAGE_BOOL;
		exp->constant.bval = true;
		return exp;
	case T_FALSE:
		exp->constant.storage = TYPE_STORAGE_BOOL;
		exp->constant.bval = false;
		return exp;
	case T_NULL:
		exp->constant.storage = TYPE_STORAGE_NULL;
		return exp;
	case T_VOID:
		exp->constant.storage = TYPE_STORAGE_VOID;
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
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_SIZE:
		exp->constant.uval = (uintmax_t)tok.uval;
		break;
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_INT:
		exp->constant.ival = (intmax_t)tok.ival;
		break;
	case TYPE_STORAGE_RUNE:
		exp->constant.rune = tok.rune;
		break;
	case TYPE_STORAGE_STRING:
		exp->constant.string.len = tok.string.len;
		exp->constant.string.value = tok.string.value;
		break;
	default:
		assert(0); // TODO
	}

	trleave(TR_PARSE, "%s", token_str(&tok));
	return exp;
}

static struct ast_expression *
parse_array_literal(struct lexer *lexer)
{
	trenter(TR_PARSE, "array-literal");

	struct token tok;
	want(lexer, T_LBRACKET, &tok);

	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_CONSTANT;
	exp->constant.storage = TYPE_STORAGE_ARRAY;

	struct ast_array_constant *item, **next = &exp->constant.array;

	while (lex(lexer, &tok) != T_RBRACKET) {
		unlex(lexer, &tok);

		item = *next = xcalloc(1, sizeof(struct ast_expression));
		item->value = parse_simple_expression(lexer);
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

	trleave(TR_PARSE, NULL);
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
	switch (lex(lexer, &tok)) {
	case T_NAME:
		name = tok.name;
		switch (lex(lexer, &tok)) {
		case T_COLON:
			exp->is_embedded = false;
			exp->field.name = name;
			exp->field.type = parse_type(lexer);
			want(lexer, T_EQUAL, NULL);
			// TODO: initializer can be allocation
			exp->field.initializer = parse_complex_expression(lexer);
			trace(TR_PARSE, "%s: [type] = [expr]", name);
			break;
		case T_EQUAL:
			exp->is_embedded = false;
			exp->field.name = name;
			// TODO: initializer can be allocation
			exp->field.initializer = parse_simple_expression(lexer);
			trace(TR_PARSE, "%s = [expr]", name);
			break;
		case T_DOUBLE_COLON:
			exp->is_embedded = true;
			struct identifier ident = {0};
			struct identifier *i = &ident;
			parse_identifier(lexer, i);
			while (i->ns != NULL) {
				i = i->ns;
			}
			i->ns = xcalloc(sizeof(struct identifier), 1);
			i->ns->name = name;
			exp->embedded = parse_struct_literal(lexer, ident);
			trace(TR_PARSE, "[embedded struct %s]",
				identifier_unparse(&ident));
			break;
		default:
			unlex(lexer, &tok);
			exp->is_embedded = true;
			ident.name = name;
			ident.ns = NULL;
			exp->embedded = parse_struct_literal(lexer, ident);
			trace(TR_PARSE, "[embedded struct %s]", name);
			break;
		}
		break;
	case T_STRUCT:
		exp->is_embedded = true;
		struct identifier id = {0};
		exp->embedded = parse_struct_literal(lexer, id);
			trace(TR_PARSE, "[embedded anonymous struct]");
		break;
	default:
		assert(0);
	}
	return exp;
}

static struct ast_expression *
parse_struct_literal(struct lexer *lexer, struct identifier ident)
{
	trenter(TR_PARSE, "struct-literal");
	want(lexer, T_LBRACE, NULL);
	struct ast_expression *exp = xcalloc(sizeof(struct ast_expression), 1);
	exp->type = EXPR_STRUCT;
	exp->_struct.type = ident;
	struct ast_field_value **next = &exp->_struct.fields;
	struct token tok = {0};
	while (tok.token != T_RBRACE) {
		switch (lex(lexer, &tok)) {
		case T_ELLIPSIS:
			trace(TR_PARSE, "...");
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
	trleave(TR_PARSE, NULL);
	return exp;
}

static struct ast_expression *
parse_plain_expression(struct lexer *lexer)
{
	trace(TR_PARSE, "plain");

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
		parse_identifier(lexer, &ident);
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
		exp = parse_complex_expression(lexer);
		want(lexer, T_RPAREN, &tok);
		return exp;
	default:
		synassert(false, &tok, T_LITERAL, T_NAME,
			T_LBRACKET, T_STRUCT, T_LPAREN, T_EOF);
	}
	assert(0); // Unreachable
}

static struct ast_expression *parse_postfix_expression(struct lexer *lexer,
		struct ast_expression *exp);

static struct ast_expression *
parse_measurement_expression(struct lexer *lexer)
{
	trace(TR_PARSE, "measurement");

	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
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
		exp->measure.value = parse_postfix_expression(lexer, NULL);
		break;
	case T_OFFSET:
		exp->measure.op = M_OFFSET;
		assert(0); // TODO
	default:
		synassert(false, &tok, T_SIZE, T_LEN, T_OFFSET, T_EOF);
	}

	want(lexer, T_RPAREN, NULL);
	return exp;
}

static struct ast_expression *
parse_call_expression(struct lexer *lexer, struct ast_expression *lvalue)
{
	trenter(TR_PARSE, "call");

	struct token tok;
	want(lexer, T_LPAREN, &tok);

	struct ast_expression *expr = xcalloc(1, sizeof(struct ast_expression));
	expr->type = EXPR_CALL;
	expr->call.lvalue = lvalue;

	struct ast_call_argument *arg, **next = &expr->call.args;
	while (lex(lexer, &tok) != T_RPAREN) {
		unlex(lexer, &tok);
		trenter(TR_PARSE, "arg");

		arg = *next = xcalloc(1, sizeof(struct ast_call_argument));
		arg->value = parse_complex_expression(lexer);

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
		trleave(TR_PARSE, NULL);
	}

	trleave(TR_PARSE, NULL);
	return expr;
}

static struct ast_expression *
parse_index_slice_expression(struct lexer *lexer, struct ast_expression *lvalue)
{
	trenter(TR_PARSE, "slice-index");

	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
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
		start = parse_simple_expression(lexer);
		break;
	}

	switch (lex(lexer, &tok)) {
	case T_SLICE:
		is_slice = true;
		break;
	case T_RBRACKET:
		break;
	default:
		synassert(false, &tok, T_SLICE, T_RBRACKET, T_EOF);
		break;
	}

	if (!is_slice) {
		exp->type = EXPR_ACCESS;
		exp->access.type = ACCESS_INDEX;
		exp->access.array = lvalue;
		exp->access.index = start;
		trleave(TR_PARSE, "slice-index (index)");
		return exp;
	}

	switch (lex(lexer, &tok)) {
	case T_RBRACKET:
		break;
	default:
		end = parse_simple_expression(lexer);
		break;
	}

	exp->type = EXPR_SLICE;
	exp->slice.object = lvalue;
	exp->slice.start = start;
	exp->slice.end = end;
	trleave(TR_PARSE, "slice-index (slice)");
	return exp;
}

static struct ast_expression *
parse_postfix_expression(struct lexer *lexer, struct ast_expression *lvalue)
{
	trace(TR_PARSE, "postfix");

	// TODO: The builtins should probably be moved outside of
	// postfix-expression in the specification
	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_ABORT:
	case T_ASSERT:
	case T_STATIC:
		assert(0); // TODO: assertion expression
	case T_SIZE:
	case T_LEN:
	case T_OFFSET:
		unlex(lexer, &tok);
		return parse_measurement_expression(lexer);
	default:
		unlex(lexer, &tok);
		break;
	}

	if (lvalue == NULL) {
		lvalue = parse_plain_expression(lexer);
	}

	switch (lex(lexer, &tok)) {
	case T_LPAREN:
		unlex(lexer, &tok);
		lvalue = parse_call_expression(lexer, lvalue);
		break;
	case T_DOT:
		trenter(TR_PARSE, "field-access");
		want(lexer, T_NAME, &tok);
		struct ast_expression *exp =
			xcalloc(sizeof(struct ast_expression), 1);
		exp->type = EXPR_ACCESS;
		exp->access.type = ACCESS_FIELD;
		exp->access._struct = lvalue;
		exp->access.field = tok.name;
		lvalue = exp;
		trleave(TR_PARSE, NULL);
		break;
	case T_LBRACKET:
		unlex(lexer, &tok);
		lvalue = parse_index_slice_expression(lexer, lvalue);
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
	trace(TR_PARSE, "object-selector");
	struct token tok;
	lex(lexer, &tok);
	unlex(lexer, &tok);
	struct ast_expression *exp = parse_postfix_expression(lexer, NULL);
	synassert_msg(exp->type == EXPR_ACCESS, "expected object", &tok);
	return exp;
}

static struct ast_expression *
parse_unary_expression(struct lexer *lexer)
{
	trace(TR_PARSE, "unary-arithmetic");

	struct token tok;
	struct ast_expression *exp;
	switch (lex(lexer, &tok)) {
	case T_PLUS:	// +
	case T_MINUS:	// -
	case T_BNOT:	// ~
	case T_LNOT:	// !
	case T_TIMES:	// *
	case T_BAND:	// &
		exp = xcalloc(1, sizeof(struct ast_expression));
		exp->type = EXPR_UNARITHM;
		exp->unarithm.op = unop_for_token(tok.token);
		if (tok.token == T_BAND) {
			exp->unarithm.operand = parse_object_selector(lexer);
		} else {
			exp->unarithm.operand = parse_unary_expression(lexer);
		}
		return exp;
	default:
		unlex(lexer, &tok);
		return parse_postfix_expression(lexer, NULL);
	}
}

static struct ast_expression *
parse_cast_expression(struct lexer *lexer)
{
	trace(TR_PARSE, "cast");
	struct ast_expression *value = parse_unary_expression(lexer);

	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_COLON:
	case T_AS:
	case T_IS:
		assert(0); // TODO
	default:
		unlex(lexer, &tok);
		return value;
	}
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
	trace(TR_PARSE, "bin-arithm");
	if (!lvalue) {
		lvalue = parse_cast_expression(lexer);
	}

	struct token tok;
	lex(lexer, &tok);

	int j;
	while ((j = precedence(tok.token)) >= i) {
		enum binarithm_operator op = binop_for_token(tok.token);

		struct ast_expression *rvalue = parse_cast_expression(lexer);
		lex(lexer, &tok);

		int k;
		while ((k = precedence(tok.token)) > j) {
			unlex(lexer, &tok);
			rvalue = parse_bin_expression(lexer, rvalue, k);
			lex(lexer, &tok);
		}

		struct ast_expression *e = xcalloc(1, sizeof(struct ast_expression));
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
parse_simple_expression(struct lexer *lexer)
{
	return parse_bin_expression(lexer, NULL, 0);
}

static struct ast_expression *
parse_if_expression(struct lexer *lexer)
{
	trenter(TR_PARSE, "if");
	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_IF;

	struct token tok = {0};

	want(lexer, T_LPAREN, &tok);
	exp->_if.cond = parse_simple_expression(lexer);
	want(lexer, T_RPAREN, &tok);

	exp->_if.true_branch = parse_compound_expression(lexer);

	switch (lex(lexer, &tok)) {
	case T_ELSE:
		if (lex(lexer, &tok) == T_IF) {
			exp->_if.false_branch = parse_if_expression(lexer);
		} else {
			unlex(lexer, &tok);
			exp->_if.false_branch = parse_compound_expression(lexer);
		}
		break;
	default:
		unlex(lexer, &tok);
		break;
	}

	trleave(TR_PARSE, NULL);
	return exp;
}

static struct ast_expression *
parse_for_expression(struct lexer *lexer)
{
	trenter(TR_PARSE, "for");
	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_FOR;

	struct token tok = {0};
	want(lexer, T_LPAREN, &tok);
	switch (lex(lexer, &tok)) {
	case T_LET:
	case T_CONST:
		unlex(lexer, &tok);
		exp->_for.bindings = parse_binding_list(lexer);
		want(lexer, T_SEMICOLON, &tok);
		break;
	default:
		unlex(lexer, &tok);
		break;
	}

	exp->_for.cond = parse_simple_expression(lexer);

	switch (lex(lexer, &tok)) {
	case T_SEMICOLON:
		exp->_for.afterthought = parse_scope_expression(lexer);
		want(lexer, T_RPAREN, &tok);
		break;
	case T_RPAREN:
		break;
	default:
		synassert(false, &tok, T_SEMICOLON, T_RPAREN, T_EOF);
	}

	exp->_for.body = parse_compound_expression(lexer);

	trleave(TR_PARSE, NULL);
	return exp;
}

static struct ast_expression *
parse_complex_expression(struct lexer *lexer)
{
	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_IF:
		return parse_if_expression(lexer);
	case T_FOR:
		return parse_for_expression(lexer);
	case T_LABEL:
		assert(0); // TODO: Loop labels
	case T_MATCH:
	case T_SWITCH:
		assert(0); // TODO
	default:
		unlex(lexer, &tok);
		return parse_simple_expression(lexer);
	}
}

static struct ast_expression *
parse_binding_list(struct lexer *lexer)
{
	trenter(TR_PARSE, "binding-list");
	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_BINDING;
	unsigned int flags = 0;

	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_CONST:
		flags = TYPE_CONST;
		// fallthrough
	case T_LET:
		// no-op
		break;
	case T_STATIC:
		assert(0); // TODO
	default:
		synassert(false, &tok, T_LET, T_CONST, T_EOF);
	}

	struct ast_expression_binding *binding = &exp->binding;
	struct ast_expression_binding **next = &exp->binding.next;

	bool more = true;
	while (more) {
		want(lexer, T_NAME, &tok);
		binding->name = tok.name;
		binding->initializer = xcalloc(1, sizeof(struct ast_expression));
		binding->flags = flags;

		switch (lex(lexer, &tok)) {
		case T_COLON:
			binding->type = parse_type(lexer);
			binding->type->flags |= flags;
			want(lexer, T_EQUAL, &tok);
			binding->initializer = parse_complex_expression(lexer);
			break;
		case T_EQUAL:
			binding->initializer = parse_simple_expression(lexer);
			break;
		default:
			synassert(false, &tok, T_COLON, T_COMMA, T_EOF);
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

	trleave(TR_PARSE, NULL);
	return exp;
}

static struct ast_expression *
parse_assignment(struct lexer *lexer, struct ast_expression *object, bool indirect)
{
	trenter(TR_PARSE, "assign");
	struct ast_expression *value = parse_complex_expression(lexer);
	struct ast_expression *expr = xcalloc(1, sizeof(struct ast_expression));
	expr->type = EXPR_ASSIGN;
	expr->assign.object = object;
	expr->assign.value = value;
	expr->assign.indirect = indirect;
	trleave(TR_PARSE, NULL);
	return expr;
}

static struct ast_expression *
parse_scope_expression(struct lexer *lexer)
{
	// This is one of the more complicated non-terminals to parse.
	struct token tok;
	bool indirect = false;
	switch (lex(lexer, &tok)) {
	case T_TIMES: // *ptr = value (or unary-expression)
		// TODO: indirect access is untested (pending support for
		// dereferencing in unary-expression)
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
		return parse_binding_list(lexer);
	case T_STATIC:
		assert(0); // TODO: This is a static binding list or assert
	case T_IF:
	case T_FOR:
	case T_MATCH:
	case T_SWITCH:
		unlex(lexer, &tok);
		value = parse_complex_expression(lexer);
		if (indirect) {
			assert(0); // TODO: Wrap value in unary dereference
		}
		return value;
	default:
		unlex(lexer, &tok);
		value = parse_unary_expression(lexer);
		if (!indirect && value->type != EXPR_ACCESS) {
			return value;
		}
		// Is possible object-selector, try for assignment
		break;
	}

	switch (lex(lexer, &tok)) {
	case T_EQUAL:
		return parse_assignment(lexer, value, indirect);
	default:
		unlex(lexer, &tok);
		value = parse_bin_expression(lexer, value, 0);
		if (indirect) {
			assert(0); // TODO: Wrap value in unary dereference
		}
		return value;
	}
}

static struct ast_expression *
parse_control_statement(struct lexer *lexer)
{
	trenter(TR_PARSE, "control-expression");

	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));

	struct token tok;
	switch (lex(lexer, &tok)) {
	case T_BREAK:
	case T_CONTINUE:
		assert(0); // TODO
	case T_RETURN:
		trace(TR_PARSE, "return");
		exp->type = EXPR_RETURN;
		exp->_return.value = NULL;
		struct token tok;
		switch (lex(lexer, &tok)) {
		case T_SEMICOLON:
			unlex(lexer, &tok);
			break;
		default:
			unlex(lexer, &tok);
			exp->_return.value = parse_complex_expression(lexer);
			break;
		}
		break;
	default:
		synassert(false, &tok, T_BREAK, T_CONTINUE, T_RETURN, T_EOF);
	}

	trleave(TR_PARSE, NULL);
	return exp;
}

static struct ast_expression *
parse_expression_list(struct lexer *lexer)
{
	trenter(TR_PARSE, "expression-list");
	want(lexer, T_LBRACE, NULL);

	struct ast_expression *exp = xcalloc(1, sizeof(struct ast_expression));
	struct ast_expression_list *cur = &exp->list;
	struct ast_expression_list **next = &cur->next;
	exp->type = EXPR_LIST;

	bool more = true;
	while (more) {
		struct token tok = {0};
		switch (lex(lexer, &tok)) {
		case T_BREAK:
		case T_CONTINUE:
		case T_RETURN:
			unlex(lexer, &tok);
			cur->expr = parse_control_statement(lexer);
			more = false;
			break;
		default:
			unlex(lexer, &tok);
			cur->expr = parse_scope_expression(lexer);
			break;
		}

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

	trleave(TR_PARSE, NULL);
	return exp;
}

static struct ast_expression *
parse_compound_expression(struct lexer *lexer)
{
	struct token tok = {0};
	switch (lex(lexer, &tok)) {
	case T_LBRACE:
		unlex(lexer, &tok);
		return parse_expression_list(lexer);
	default:
		unlex(lexer, &tok);
		return parse_simple_expression(lexer);
	}
}

static char *
parse_attr_symbol(struct lexer *lexer)
{
	struct token tok = {0};
	want(lexer, T_LPAREN, NULL);
	want(lexer, T_LITERAL, &tok);
	synassert_msg(tok.storage == TYPE_STORAGE_STRING,
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
	trenter(TR_PARSE, "global");
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
		}
		parse_identifier(lexer, &i->ident);
		want(lexer, T_COLON, NULL);
		i->type = parse_type(lexer);
		if (mode == T_CONST) {
			i->type->flags |= TYPE_CONST;
		}
		want(lexer, T_EQUAL, NULL);
		i->init = parse_simple_expression(lexer);
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			lex(lexer, &tok);
			if (tok.token == T_NAME || tok.token == T_ATTR_SYMBOL) {
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

	for (struct ast_global_decl *i = decl; i; i = i->next) {
		char buf[1024];
		identifier_unparse_static(&i->ident, buf, sizeof(buf));
		if (decl->symbol) {
			trace(TR_PARSE, "%s @symbol(\"%s\") %s: [type] = [expr]",
				lexical_token_str(mode), decl->symbol, buf);
		} else {
			trace(TR_PARSE, "%s %s: [type] = [expr]",
				lexical_token_str(mode), buf);
		}
	}
	trleave(TR_PARSE, NULL);
}

static void
parse_type_decl(struct lexer *lexer, struct ast_type_decl *decl)
{
	trenter(TR_PARSE, "typedef");
	struct token tok = {0};
	struct ast_type_decl *i = decl;
	bool more = true;
	while (more) {
		parse_identifier(lexer, &i->ident);
		want(lexer, T_EQUAL, NULL);
		i->type = parse_type(lexer);
		switch (lex(lexer, &tok)) {
		case T_COMMA:
			lex(lexer, &tok);
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

	for (struct ast_type_decl *i = decl; i; i = i->next) {
		char ibuf[1024], tbuf[1024];
		identifier_unparse_static(&i->ident, ibuf, sizeof(ibuf));
		strncpy(tbuf, "[type]", sizeof(tbuf)); // TODO: unparse type
		trace(TR_PARSE, "def %s = %s", ibuf, tbuf);
	}
	trleave(TR_PARSE, NULL);
}

static void
parse_fn_decl(struct lexer *lexer, struct ast_function_decl *decl)
{
	trenter(TR_PARSE, "fn");
	struct token tok = {0};
	bool more = true;
	bool noreturn = false;
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
	parse_identifier(lexer, &decl->ident);
	parse_prototype(lexer, &decl->prototype);
	if (noreturn) {
		decl->prototype.flags |= FN_NORETURN;
	}

	switch (lex(lexer, &tok)) {
	case T_EQUAL:
		decl->body = parse_compound_expression(lexer);
		break;
	case T_SEMICOLON:
		unlex(lexer, &tok);
		decl->body = NULL; // Prototype
		break;
	default:
		synassert(false, &tok, T_EQUAL, T_SEMICOLON, T_EOF);
	}

	char symbol[1024], buf[1024];
	if (decl->symbol) {
		snprintf(symbol, sizeof(symbol), "@symbol(\"%s\") ", decl->symbol);
	}
	identifier_unparse_static(&decl->ident, buf, sizeof(buf));
	trace(TR_PARSE, "%s%s%s%s%sfn %s [prototype] = [expr]",
		decl->flags & FN_FINI ? "@fini " : "",
		decl->flags & FN_INIT ? "@init " : "",
		decl->prototype.flags & FN_NORETURN ? "@noreturn " : "",
		decl->flags & FN_TEST ? "@test " : "",
		decl->symbol ? symbol : "", buf);
	trleave(TR_PARSE, NULL);
}

static void
parse_decl(struct lexer *lexer, struct ast_decl *decl)
{
	struct token tok = {0};
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
parse_decls(struct lexer *lexer, struct ast_decls *decls)
{
	trenter(TR_PARSE, "decls");
	struct token tok = {0};
	struct ast_decls **next = &decls;
	while (tok.token != T_EOF) {
		switch (lex(lexer, &tok)) {
		case T_EXPORT:
			(*next)->decl.exported = true;
			trace(TR_PARSE, "export");
			break;
		default:
			unlex(lexer, &tok);
			break;
		}
		parse_decl(lexer, &(*next)->decl);
		next = &(*next)->next;
		*next = xcalloc(1, sizeof(struct ast_decls));
		want(lexer, T_SEMICOLON, NULL);
		if (lex(lexer, &tok) != T_EOF) {
			unlex(lexer, &tok);
		}
	}
	free(*next);
	*next = 0;
	trleave(TR_PARSE, NULL);
}

void
parse(struct lexer *lexer, struct ast_subunit *subunit)
{
	parse_imports(lexer, subunit);
	parse_decls(lexer, &subunit->decls);
	want(lexer, T_EOF, NULL);
}
