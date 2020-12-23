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

struct parser {
	struct lexer *lex;
};

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
want(struct parser *par, enum lexical_token ltok, struct token *tok)
{
	struct token _tok = {0};
	struct token *out = tok ? tok : &_tok;
	lex(par->lex, out);
	synassert(out->token == ltok, out, ltok, T_EOF);
	if (!tok) {
		token_finish(out);
	}
}

static void
parse_identifier(struct parser *par, struct identifier *ident)
{
	struct token tok = {0};
	struct identifier *i = ident;
	trenter(TR_PARSE, "identifier");

	while (!i->name) {
		want(par, T_NAME, &tok);
		i->name = strdup(tok.name);
		token_finish(&tok);

		struct identifier *ns;
		switch (lex(par->lex, &tok)) {
		case T_DOUBLE_COLON:
			ns = calloc(1, sizeof(struct identifier));
			*ns = *i;
			i->ns = ns;
			i->name = NULL;
			break;
		default:
			unlex(par->lex, &tok);
			break;
		}
	}

	char buf[1024];
	identifier_unparse_static(ident, buf, sizeof(buf));
	trleave(TR_PARSE, "%s", buf);
}

static void
parse_import(struct parser *par, struct ast_imports *imports)
{
	trenter(TR_PARSE, "import");
	struct identifier ident = {0};
	parse_identifier(par, &ident);

	struct token tok = {0};
	switch (lex(par->lex, &tok)) {
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
parse_imports(struct parser *par, struct ast_subunit *subunit)
{
	trenter(TR_PARSE, "imports");
	struct token tok = {0};
	struct ast_imports **next = &subunit->imports;

	bool more = true;
	while (more) {
		struct ast_imports *imports;
		switch (lex(par->lex, &tok)) {
		case T_USE:
			imports = calloc(1, sizeof(struct ast_imports));
			parse_import(par, imports);
			*next = imports;
			next = &imports->next;
			break;
		default:
			unlex(par->lex, &tok);
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

static void parse_type(struct parser *par, struct ast_type *type);

static void
parse_parameter_list(struct parser *par, struct ast_function_type *type)
{
	trenter(TR_PARSE, "parameter-list");
	struct token tok = {0};
	bool more = true;
	struct ast_function_parameters **next = &type->params;
	while (more) {
		*next = calloc(1, sizeof(struct ast_function_parameters));
		(*next)->type = calloc(1, sizeof(struct ast_type));
		want(par, T_NAME, &tok);
		(*next)->name = tok.name;
		want(par, T_COLON, NULL);
		parse_type(par, (*next)->type);
		trace(TR_PARSE, "%s: [type]", (*next)->name);
		switch (lex(par->lex, &tok)) {
		case T_COMMA:
			switch (lex(par->lex, &tok)) {
			case T_ELLIPSIS:
				type->variadism = VARIADISM_HARE;
				if (lex(par->lex, &tok) != T_COMMA) {
					unlex(par->lex, &tok);
				}
				more = false;
				trace(TR_PARSE, ", ...");
				break;
			default:
				unlex(par->lex, &tok);
				next = &(*next)->next;
				break;
			}
			break;
		case T_ELLIPSIS:
			type->variadism = VARIADISM_C;
			if (lex(par->lex, &tok) != T_COMMA) {
				unlex(par->lex, &tok);
			}
			more = false;
			trace(TR_PARSE, "...");
			break;
		default:
			more = false;
			unlex(par->lex, &tok);
			break;
		}
	}
	trleave(TR_PARSE, NULL);
}

static void
parse_prototype(struct parser *par, struct ast_function_type *type)
{
	trenter(TR_PARSE, "prototype");
	want(par, T_LPAREN, NULL);
	struct token tok = {0};
	if (lex(par->lex, &tok) != T_RPAREN) {
		unlex(par->lex, &tok);
		parse_parameter_list(par, type);
		want(par, T_RPAREN, NULL);
	}
	type->result = calloc(1, sizeof(struct ast_type));
	parse_type(par, type->result);
	size_t ctr = 0;
	for (struct ast_function_parameters *param = type->params;
			param; param = param->next) {
		ctr++;
	}
	trace(TR_PARSE, "[%zu parameters] [type]", ctr);
	trleave(TR_PARSE, NULL);
}

static void
parse_integer_type(struct parser *par, enum type_storage *storage) {
	trenter(TR_PARSE, "integer");
	struct token tok = {0};
	switch (lex(par->lex, &tok)) {
	case T_I8:
		*storage = TYPE_STORAGE_I8;
		break;
	case T_I16:
		*storage = TYPE_STORAGE_I16;
		break;
	case T_I32:
		*storage = TYPE_STORAGE_I32;
		break;
	case T_I64:
		*storage = TYPE_STORAGE_I64;
		break;
	case T_U8:
		*storage = TYPE_STORAGE_U8;
		break;
	case T_U16:
		*storage = TYPE_STORAGE_U16;
		break;
	case T_U32:
		*storage = TYPE_STORAGE_U32;
		break;
	case T_U64:
		*storage = TYPE_STORAGE_U64;
		break;
	case T_INT:
		*storage = TYPE_STORAGE_INT;
		break;
	case T_UINT:
		*storage = TYPE_STORAGE_UINT;
		break;
	case T_SIZE:
		*storage = TYPE_STORAGE_SIZE;
		break;
	case T_UINTPTR:
		*storage = TYPE_STORAGE_UINTPTR;
		break;
	case T_CHAR:
		*storage = TYPE_STORAGE_CHAR;
		break;
	default:
		assert(0);
	}
	trleave(TR_PARSE, "%s", type_storage_unparse(*storage));
}

static struct ast_expression *parse_simple_expression(struct parser *par);

static void
parse_enum_type(struct parser *par, struct ast_enum_type *type)
{
	trenter(TR_PARSE, "enum");
	struct token tok = {0};
	switch (lex(par->lex, &tok)) {
	case T_LBRACE:
		type->storage = TYPE_STORAGE_INT;
		unlex(par->lex, &tok);
		break;
	default:
		unlex(par->lex, &tok);
		parse_integer_type(par, &type->storage);
		break;
	}
	want(par, T_LBRACE, NULL);
	struct ast_enum_field **next = &type->values;
	while (tok.token != T_RBRACE) {
		*next = calloc(1, sizeof(struct ast_enum_field));
		want(par, T_NAME, &tok);
		(*next)->name = tok.name;
		if (lex(par->lex, &tok) == T_EQUAL) {
			(*next)->value = parse_simple_expression(par);
			trace(TR_PARSE, "%s = [expr]", (*next)->name);
		} else {
			unlex(par->lex, &tok);
			trace(TR_PARSE, "%s = [generated]", (*next)->name);
		}
		next = &(*next)->next;
		switch (lex(par->lex, &tok)) {
		case T_COMMA:
			if (lex(par->lex, &tok) != T_RBRACE) {
				unlex(par->lex, &tok);
			}
		case T_RBRACE:
			break;
		default:
			synassert(false, &tok, T_COMMA, T_RBRACE, T_EOF);
		}
	}
	trleave(TR_PARSE, NULL);
}

static void
parse_type(struct parser *par, struct ast_type *type)
{
	trenter(TR_PARSE, "type");
	struct token tok = {0};
	switch (lex(par->lex, &tok)) {
	case T_CONST:
		type->flags |= TYPE_CONST;
		break;
	default:
		unlex(par->lex, &tok);
		break;
	}
	switch (lex(par->lex, &tok)) {
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
		unlex(par->lex, &tok);
		parse_integer_type(par, &type->storage);
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
	case T_ENUM:
		type->storage = TYPE_STORAGE_ENUM;
		parse_enum_type(par, &type->_enum);
		break;
	case T_NULLABLE:
		type->pointer.flags |= PTR_NULLABLE;
		want(par, T_TIMES, NULL);
		trace(TR_PARSE, "nullable");
		/* fallthrough */
	case T_TIMES:
		type->storage = TYPE_STORAGE_POINTER;
		type->pointer.referent = calloc(1, sizeof(struct ast_type));
		parse_type(par, type->pointer.referent);
		break;
	case T_STRUCT:
	case T_UNION:
		assert(0); // TODO: Structs/unions
	case T_LPAREN:
		assert(0); // TODO: Tagged unions
	case T_LBRACKET:
		assert(0); // TODO: Slices/arrays
	case T_ATTR_NORETURN:
		type->func.flags |= FN_NORETURN;
		want(par, T_FN, NULL);
		/* fallthrough */
	case T_FN:
		type->storage = TYPE_STORAGE_FUNCTION;
		parse_prototype(par, &type->func);
		break;
	case T_NAME:
		unlex(par->lex, &tok);
		type->storage = TYPE_STORAGE_ALIAS;
		parse_identifier(par, &type->alias);
		break;
	default:
		synassert_msg(false, "expected type", &tok);
		break;
	}
	trleave(TR_PARSE, "%s%s", type->flags & TYPE_CONST ? "const " : "",
		type_storage_unparse(type->storage));
}

static struct ast_expression *parse_complex_expression(struct parser *par);

static struct ast_expression *
parse_access(struct parser *par)
{
	trace(TR_PARSE, "access");
	struct ast_expression *exp = calloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_ACCESS;
	parse_identifier(par, &exp->access.ident);
	return exp;
}

static struct ast_expression *
parse_constant(struct parser *par)
{
	trenter(TR_PARSE, "constant");

	struct token tok = {0};
	want(par, T_LITERAL, &tok);

	struct ast_expression *exp = calloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_CONSTANT;
	exp->constant.storage = tok.storage;

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
parse_plain_expression(struct parser *par)
{
	trace(TR_PARSE, "plain");

	struct token tok;
	switch (lex(par->lex, &tok)) {
	case T_LITERAL:
		unlex(par->lex, &tok);
		return parse_constant(par);
	case T_NAME:
		unlex(par->lex, &tok);
		return parse_access(par);
	case T_LBRACKET:
		assert(0); // TODO: Array literal
	case T_STRUCT:
		assert(0); // TODO: Struct literal
	default:
		synassert(false, &tok, T_LITERAL, T_NAME,
			T_LBRACKET, T_STRUCT, T_EOF);
	}
	assert(0); // Unreachable
}

static struct ast_expression *
parse_postfix_expression(struct parser *par)
{
	trace(TR_PARSE, "postfix");
	struct ast_expression *lvalue;

	struct token tok;
	switch (lex(par->lex, &tok)) {
	case T_ABORT:
	case T_ASSERT:
	case T_STATIC:
		assert(0); // TODO: assertion expression
	case T_SIZE:
	case T_LEN:
	case T_OFFSET:
		assert(0); // TODO: measurement expression
	case T_LPAREN:
		lvalue = parse_complex_expression(par);
		want(par, T_LPAREN, &tok);
		break;
	default:
		unlex(par->lex, &tok);
		lvalue = parse_plain_expression(par);
		break;
	}

	switch (lex(par->lex, &tok)) {
	case T_LPAREN:
		assert(0); // TODO: call expression
	case T_DOT:
		assert(0); // TODO: field access expression
	case T_LBRACKET:
		assert(0); // TODO: indexing/slicing expression
	default:
		unlex(par->lex, &tok);
		return lvalue;
	}

	assert(0); // Unreachable
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
parse_object_selector(struct parser *par)
{
	trace(TR_PARSE, "object-selector");
	struct token tok;
	lex(par->lex, &tok);
	unlex(par->lex, &tok);
	struct ast_expression *exp = parse_postfix_expression(par);
	synassert_msg(exp->type == EXPR_ACCESS, "expected object", &tok);
	return exp;
}

static struct ast_expression *
parse_unary_expression(struct parser *par)
{
	trace(TR_PARSE, "unary-arithmetic");

	struct token tok;
	struct ast_expression *exp;
	switch (lex(par->lex, &tok)) {
	case T_PLUS:	// +
	case T_MINUS:	// -
	case T_BNOT:	// ~
	case T_LNOT:	// !
	case T_TIMES:	// *
	case T_BAND:	// &
		exp = calloc(1, sizeof(struct ast_expression));
		exp->type = EXPR_UNARITHM;
		exp->unarithm.op = unop_for_token(tok.token);
		if (tok.token == T_BAND) {
			exp->unarithm.operand = parse_object_selector(par);
		} else {
			exp->unarithm.operand = parse_unary_expression(par);
		}
		return exp;
	default:
		unlex(par->lex, &tok);
		return parse_postfix_expression(par);
	}
}

static struct ast_expression *
parse_cast_expression(struct parser *par)
{
	trace(TR_PARSE, "cast");
	struct ast_expression *value = parse_unary_expression(par);

	struct token tok;
	switch (lex(par->lex, &tok)) {
	case T_COLON:
	case T_AS:
	case T_IS:
		assert(0); // TODO
	default:
		unlex(par->lex, &tok);
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
parse_bin_expression(struct parser *par, struct ast_expression *lvalue, int i)
{
	trace(TR_PARSE, "bin-arithm");
	if (!lvalue) {
		lvalue = parse_cast_expression(par);
	}

	struct token tok;
	lex(par->lex, &tok);

	int j;
	while ((j = precedence(tok.token)) >= i) {
		enum binarithm_operator op = binop_for_token(tok.token);

		struct ast_expression *rvalue = parse_cast_expression(par);
		lex(par->lex, &tok);

		int k;
		while ((k = precedence(tok.token)) > j) {
			unlex(par->lex, &tok);
			rvalue = parse_bin_expression(par, rvalue, k);
			lex(par->lex, &tok);
		}

		struct ast_expression *e = calloc(1, sizeof(struct ast_expression));
		e->type = EXPR_BINARITHM;
		e->binarithm.op = op;
		e->binarithm.lvalue = lvalue;
		e->binarithm.rvalue = rvalue;
		lvalue = e;
	}

	unlex(par->lex, &tok);
	return lvalue;
}

static struct ast_expression *
parse_simple_expression(struct parser *par)
{
	return parse_bin_expression(par, NULL, 0);
}

static struct ast_expression *
parse_complex_expression(struct parser *par)
{
	struct token tok;
	switch (lex(par->lex, &tok)) {
	case T_IF:
	case T_FOR:
	case T_MATCH:
	case T_SWITCH:
		assert(0); // TODO
	default:
		unlex(par->lex, &tok);
		return parse_simple_expression(par);
	}
}

static struct ast_expression *
parse_binding_list(struct parser *par)
{
	trenter(TR_PARSE, "binding-list");
	struct ast_expression *exp = calloc(1, sizeof(struct ast_expression));
	exp->type = EXPR_BINDING;
	unsigned int flags = 0;

	struct token tok;
	switch (lex(par->lex, &tok)) {
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
		want(par, T_NAME, &tok);
		binding->name = tok.name;
		binding->initializer = calloc(1, sizeof(struct ast_expression));
		binding->flags = flags;

		switch (lex(par->lex, &tok)) {
		case T_COLON:
			binding->type = calloc(1, sizeof(struct ast_type));
			parse_type(par, binding->type);
			binding->type->flags |= flags;
			want(par, T_EQUAL, &tok);
			binding->initializer = parse_complex_expression(par);
			break;
		case T_EQUAL:
			binding->initializer = parse_simple_expression(par);
			break;
		default:
			synassert(false, &tok, T_COLON, T_COMMA, T_EOF);
		}

		switch (lex(par->lex, &tok)) {
		case T_COMMA:
			*next = calloc(1, sizeof(struct ast_expression_binding));
			binding = *next;
			next = &binding->next;
			break;
		default:
			unlex(par->lex, &tok);
			more = false;
			break;
		}
	}

	trleave(TR_PARSE, NULL);
	return exp;
}

static struct ast_expression *
parse_assignment(struct parser *par, struct ast_expression *object, bool indirect)
{
	trenter(TR_PARSE, "assign");
	struct ast_expression *value = parse_complex_expression(par);
	struct ast_expression *expr = calloc(1, sizeof(struct ast_expression));
	expr->type = EXPR_ASSIGN;
	expr->assign.object = object;
	expr->assign.value = value;
	expr->assign.indirect = indirect;
	trleave(TR_PARSE, NULL);
	return expr;
}

static struct ast_expression *
parse_scope_expression(struct parser *par)
{
	// This is one of the more complicated non-terminals to parse.
	struct token tok;
	bool indirect = false;
	switch (lex(par->lex, &tok)) {
	case T_TIMES: // *ptr = value (or unary-expression)
		// TODO: indirect access is untested (pending support for
		// dereferencing in unary-expression)
		indirect = true;
		break;
	default:
		unlex(par->lex, &tok);
		break;
	}

	struct ast_expression *value;
	switch (lex(par->lex, &tok)) {
	case T_LET:
	case T_CONST:
		unlex(par->lex, &tok);
		return parse_binding_list(par);
	case T_STATIC:
		assert(0); // TODO: This is a static binding list or assert
	case T_IF:
	case T_FOR:
	case T_MATCH:
	case T_SWITCH:	// complex-expression
	case T_PLUS:
	case T_MINUS:
	case T_BNOT:
	case T_LNOT:
	case T_BAND:
	case T_TIMES:	// unary-expression
		unlex(par->lex, &tok);
		value = parse_complex_expression(par);
		if (indirect) {
			assert(0); // TODO: Wrap value in unary dereference
		}
		return value;
	default:	// postfix-expression
		unlex(par->lex, &tok);
		if (indirect) {
			value = parse_unary_expression(par);
			break;
		}
		value = parse_postfix_expression(par);
		if (value->type != EXPR_ACCESS) {
			return value;
		}
		// Is possible object-selector, try for assignment
		break;
	}

	switch (lex(par->lex, &tok)) {
	case T_EQUAL:
		return parse_assignment(par, value, indirect);
	default:
		unlex(par->lex, &tok);
		value = parse_bin_expression(par, value, 0);
		if (indirect) {
			assert(0); // TODO: Wrap value in unary dereference
		}
		return value;
	}
}

static struct ast_expression *
parse_control_statement(struct parser *par)
{
	trenter(TR_PARSE, "control-expression");

	struct ast_expression *exp = calloc(1, sizeof(struct ast_expression));

	struct token tok;
	switch (lex(par->lex, &tok)) {
	case T_BREAK:
	case T_CONTINUE:
		assert(0); // TODO
	case T_RETURN:
		trace(TR_PARSE, "return");
		exp->type = EXPR_RETURN;
		exp->_return.value = NULL;
		struct token tok;
		switch (lex(par->lex, &tok)) {
		case T_SEMICOLON:
			unlex(par->lex, &tok);
			break;
		default:
			unlex(par->lex, &tok);
			exp->_return.value = parse_complex_expression(par);
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
parse_expression_list(struct parser *par)
{
	trenter(TR_PARSE, "expression-list");
	want(par, T_LBRACE, NULL);

	struct ast_expression *exp = calloc(1, sizeof(struct ast_expression));
	struct ast_expression_list *cur = &exp->list;
	struct ast_expression_list **next = &cur->next;
	exp->type = EXPR_LIST;

	bool more = true;
	while (more) {
		struct token tok = {0};
		switch (lex(par->lex, &tok)) {
		case T_BREAK:
		case T_CONTINUE:
		case T_RETURN:
			unlex(par->lex, &tok);
			cur->expr = parse_control_statement(par);
			more = false;
			break;
		default:
			unlex(par->lex, &tok);
			cur->expr = parse_scope_expression(par);
			break;
		}

		want(par, T_SEMICOLON, &tok);

		if (more) {
			lex(par->lex, &tok);
			if (tok.token == T_RBRACE) {
				more = false;
			} else {
				unlex(par->lex, &tok);
				*next = calloc(1, sizeof(struct ast_expression_list));
				cur = *next;
				next = &cur->next;
			}
		} else {
			want(par, T_RBRACE, &tok);
		}
	}

	trleave(TR_PARSE, NULL);
	return exp;
}

static struct ast_expression *
parse_compound_expression(struct parser *par)
{
	struct token tok = {0};
	switch (lex(par->lex, &tok)) {
	case T_LBRACE:
		unlex(par->lex, &tok);
		return parse_expression_list(par);
	default:
		unlex(par->lex, &tok);
		return parse_simple_expression(par);
	}
}

static char *
parse_attr_symbol(struct parser *par)
{
	struct token tok = {0};
	want(par, T_LPAREN, NULL);
	want(par, T_LITERAL, &tok);
	synassert_msg(tok.storage == TYPE_STORAGE_STRING,
		"expected string literal", &tok);
	for (size_t i = 0; i < tok.string.len; i++) {
		uint32_t c = tok.string.value[i];
		synassert_msg(c <= 0x7F && (isalnum(c) || c == '_' || c == '$'
			|| c == '.'), "invalid symbol", &tok);
		synassert_msg(i != 0 || (!isdigit(c) && c != '$'),
			"invalid symbol", &tok);
	}
	want(par, T_RPAREN, NULL);
	return tok.string.value;
}

static void
parse_global_decl(struct parser *par, enum lexical_token mode,
		struct ast_global_decl *decl)
{
	trenter(TR_PARSE, "global");
	struct token tok = {0};
	struct ast_global_decl *i = decl;
	assert(mode == T_LET || mode == T_CONST || mode == T_DEF);
	bool more = true;
	while (more) {
		if (mode == T_LET || mode == T_CONST) {
			switch (lex(par->lex, &tok)) {
			case T_ATTR_SYMBOL:
				i->symbol = parse_attr_symbol(par);
				break;
			default:
				unlex(par->lex, &tok);
				break;
			}
		}
		parse_identifier(par, &i->ident);
		want(par, T_COLON, NULL);
		parse_type(par, &i->type);
		if (mode == T_CONST) {
			i->type.flags |= TYPE_CONST;
		}
		want(par, T_EQUAL, NULL);
		i->init = parse_simple_expression(par);
		switch (lex(par->lex, &tok)) {
		case T_COMMA:
			lex(par->lex, &tok);
			if (tok.token == T_NAME || tok.token == T_ATTR_SYMBOL) {
				i->next = calloc(1, sizeof(struct ast_global_decl));
				i = i->next;
				unlex(par->lex, &tok);
				break;
			}
			/* fallthrough */
		default:
			more = false;
			unlex(par->lex, &tok);
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
parse_type_decl(struct parser *par, struct ast_type_decl *decl)
{
	trenter(TR_PARSE, "typedef");
	struct token tok = {0};
	struct ast_type_decl *i = decl;
	bool more = true;
	while (more) {
		parse_identifier(par, &i->ident);
		want(par, T_EQUAL, NULL);
		parse_type(par, &i->type);
		switch (lex(par->lex, &tok)) {
		case T_COMMA:
			lex(par->lex, &tok);
			if (lex(par->lex, &tok) == T_NAME) {
				i->next = calloc(1, sizeof(struct ast_type_decl));
				i = i->next;
				unlex(par->lex, &tok);
				break;
			}
			/* fallthrough */
		default:
			more = false;
			unlex(par->lex, &tok);
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
parse_fn_decl(struct parser *par, struct ast_function_decl *decl)
{
	trenter(TR_PARSE, "fn");
	struct token tok = {0};
	bool more = true;
	while (more) {
		switch (lex(par->lex, &tok)) {
		case T_ATTR_FINI:
			decl->flags |= FN_FINI;
			break;
		case T_ATTR_INIT:
			decl->flags |= FN_INIT;
			break;
		case T_ATTR_SYMBOL:
			decl->symbol = parse_attr_symbol(par);
			break;
		case T_ATTR_TEST:
			decl->flags |= FN_TEST;
			break;
		case T_ATTR_NORETURN:
			decl->prototype.flags |= FN_NORETURN;
			break;
		default:
			more = false;
			unlex(par->lex, &tok);
			break;
		}
	}
	want(par, T_FN, NULL);
	parse_identifier(par, &decl->ident);
	parse_prototype(par, &decl->prototype);
	want(par, T_EQUAL, NULL);
	decl->body = parse_compound_expression(par);

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
parse_decl(struct parser *par, struct ast_decl *decl)
{
	struct token tok = {0};
	switch (lex(par->lex, &tok)) {
	case T_CONST:
	case T_LET:
		decl->decl_type = AST_DECL_GLOBAL;
		parse_global_decl(par, tok.token, &decl->global);
		break;
	case T_DEF:
		decl->decl_type = AST_DECL_CONST;
		parse_global_decl(par, tok.token, &decl->constant);
		break;
	case T_TYPE:
		decl->decl_type = AST_DECL_TYPE;
		parse_type_decl(par, &decl->type);
		break;
	default:
		unlex(par->lex, &tok);
		decl->decl_type = AST_DECL_FUNC;
		parse_fn_decl(par, &decl->function);
		break;
	}
}

static void
parse_decls(struct parser *par, struct ast_decls *decls)
{
	trenter(TR_PARSE, "decls");
	struct token tok = {0};
	struct ast_decls **next = &decls;
	while (tok.token != T_EOF) {
		switch (lex(par->lex, &tok)) {
		case T_EXPORT:
			(*next)->decl.exported = true;
			trace(TR_PARSE, "export");
			break;
		default:
			unlex(par->lex, &tok);
			break;
		}
		parse_decl(par, &(*next)->decl);
		next = &(*next)->next;
		*next = calloc(1, sizeof(struct ast_decls));
		want(par, T_SEMICOLON, NULL);
		if (lex(par->lex, &tok) != T_EOF) {
			unlex(par->lex, &tok);
		}
	}
	free(*next);
	*next = 0;
	trleave(TR_PARSE, NULL);
}

void
parse(struct lexer *lex, struct ast_subunit *subunit)
{
	struct parser par = {
		.lex = lex,
	};
	parse_imports(&par, subunit);
	parse_decls(&par, &subunit->decls);
	want(&par, T_EOF, NULL);
}
