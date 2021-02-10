// XXX: This whole file should be removed once it ceases to be useful
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "check.h"
#include "dump.h"
#include "expr.h"
#include "identifier.h"
#include "scope.h"
#include "types.h"

static void
indent(int depth)
{
	for (int i = 0; i < depth; ++i) {
		fprintf(stderr, "  ");
	}
}

static const char *
storage_to_suffix(enum type_storage storage)
{
	switch (storage) {
	case TYPE_STORAGE_F32:
		return "f32";
	case TYPE_STORAGE_F64:
		return "f64";
	case TYPE_STORAGE_I16:
		return "i16";
	case TYPE_STORAGE_I32:
		return "i32";
	case TYPE_STORAGE_I64:
		return "i64";
	case TYPE_STORAGE_I8:
		return "i8";
	case TYPE_STORAGE_INT:
		return "i";
	case TYPE_STORAGE_SIZE:
		return "z";
	case TYPE_STORAGE_U16:
		return "u16";
	case TYPE_STORAGE_U32:
		return "u32";
	case TYPE_STORAGE_U64:
		return "u64";
	case TYPE_STORAGE_U8:
		return "u8";
	case TYPE_STORAGE_UINT:
		return "u";
	case TYPE_STORAGE_UINTPTR:
		return "u64: uintptr";
	default:
		assert(0);
	}
}

static void
dump_const(const struct expression *expr)
{
	assert(expr->type == EXPR_CONSTANT);
	const union expression_constant *val = &expr->constant;
	switch (expr->result->storage) {
	case TYPE_STORAGE_BOOL:
		fprintf(stderr, "%s", val->bval ? "false" : "true");
		break;
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		fprintf(stderr, "%lf%s", val->fval,
			storage_to_suffix(expr->result->storage));
		break;
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_INT:
		fprintf(stderr, "%ld%s", val->ival,
			storage_to_suffix(expr->result->storage));
		break;
	case TYPE_STORAGE_NULL:
		fprintf(stderr, "null");
		break;
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
		fprintf(stderr, "%lu%s", val->uval,
			storage_to_suffix(expr->result->storage));
		break;
	case TYPE_STORAGE_VOID:
		fprintf(stderr, "void");
		break;
	case TYPE_STORAGE_RUNE:
		assert(0); // TODO
	case TYPE_STORAGE_ALIAS:
		fprintf(stderr, "(const) %s",
			identifier_unparse(&expr->result->alias.ident));
		break;
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TUPLE:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_TAGGED:
		assert(0); // Invariant
	}
}

static void
dump_type(const struct type *type)
{
	if (type->flags & TYPE_CONST) {
		fprintf(stderr, "const ");
	}

	char *ident;
	switch (type->storage) {
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_INT:
	case TYPE_STORAGE_NULL:
	case TYPE_STORAGE_RUNE:
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
	case TYPE_STORAGE_VOID:
	case TYPE_STORAGE_STRING:
		fprintf(stderr, "%s", type_storage_unparse(type->storage));
		break;
	case TYPE_STORAGE_POINTER:
		fprintf(stderr, "%s*", type->pointer.flags & PTR_NULLABLE
				? "nullable " : "");
		dump_type(type->pointer.referent);
		break;
	case TYPE_STORAGE_ARRAY:
		if (type->array.length == SIZE_UNDEFINED) {
			fprintf(stderr, "[*]");
		} else {
			fprintf(stderr, "[%zd]", type->array.length);
		}
		dump_type(type->array.members);
		break;
	case TYPE_STORAGE_SLICE:
		fprintf(stderr, "[]");
		dump_type(type->array.members);
		break;
	case TYPE_STORAGE_ALIAS:
		ident = identifier_unparse(&type->alias.ident);
		fprintf(stderr, "%s", ident);
		free(ident);
		break;
	case TYPE_STORAGE_TAGGED:
		fprintf(stderr, "(");
		for (const struct type_tagged_union *tu = &type->tagged;
				tu; tu = tu->next) {
			dump_type(tu->type);
			if (tu->next) {
				fprintf(stderr, " | ");
			}
		}
		fprintf(stderr, ")");
		break;
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		assert(type->struct_union.c_compat); // TODO
		fprintf(stderr, "%s { ", type->storage == TYPE_STORAGE_STRUCT
				? "struct" : "union");
		for (const struct struct_field *f = type->struct_union.fields;
				f; f = f->next) {
			fprintf(stderr, "%s: ", f->name);
			dump_type(f->type);
			fprintf(stderr, ", ");
		}
		fprintf(stderr, "}");
		break;
	case TYPE_STORAGE_FUNCTION:
		if (type->func.flags & FN_NORETURN) {
			fprintf(stderr, "@noreturn ");
		}
		fprintf(stderr, "fn(");
		for (const struct type_func_param *param = type->func.params;
				param; param = param->next) {
			if (param->next) {
				dump_type(param->type);
				fprintf(stderr, ", ");
			} else if (type->func.variadism == VARIADISM_HARE) {
				dump_type(param->type->array.members);
				fprintf(stderr, "...");
			} else if (type->func.variadism == VARIADISM_C) {
				dump_type(param->type);
				fprintf(stderr, ", ...");
			}
		}
		fprintf(stderr, ") ");
		dump_type(type->func.result);
		break;
	case TYPE_STORAGE_TUPLE:
		assert(0); // TODO
	}
}

static void
dump_scope_obj(const struct scope_object *obj)
{
	switch (obj->otype) {
	case O_BIND:
		fprintf(stderr, "{binding ");
		break;
	case O_CONST:
		fprintf(stderr, "{constant ");
		break;
	case O_DECL:
		fprintf(stderr, "{declaration ");
		break;
	case O_TYPE:
		fprintf(stderr, "{type ");
		break;
	}
	fprintf(stderr, "ident %s; name %s ",
		identifier_unparse(&obj->ident),
		identifier_unparse(&obj->name));
	dump_type(obj->type);
	fprintf(stderr, "}");
}

static void
dump_expr(const struct expression *expr, int depth)
{
	fprintf(stderr, "expr ");
	dump_type(expr->result);
	fprintf(stderr, " <- ");
	switch (expr->type) {
	case EXPR_ACCESS:
		fprintf(stderr, "access ");
		switch (expr->access.type) {
		case ACCESS_IDENTIFIER:
			fprintf(stderr, "object ");
			dump_scope_obj(expr->access.object);
			break;
		case ACCESS_INDEX:
			assert(0);
		case ACCESS_FIELD:
			assert(0);
		case ACCESS_TUPLE:
			assert(0);
		}
		break;
	case EXPR_ALLOC:
		fprintf(stderr, "alloc");
		break;
	case EXPR_APPEND:
		fprintf(stderr, "append");
		break;
	case EXPR_ASSERT:
		fprintf(stderr, "assert");
		break;
	case EXPR_ASSIGN:
		fprintf(stderr, "assign [indirect: %d; op: %d] ",
				expr->assign.indirect,
				expr->assign.op);
		dump_expr(expr->assign.object, depth);
		fprintf(stderr, " <- ");
		dump_expr(expr->assign.value, depth);
		break;
	case EXPR_BINARITHM:
		fprintf(stderr, "binarithm");
		break;
	case EXPR_BINDING:
		fprintf(stderr, "binding\n");
		indent(depth + 1);
		for (const struct expression_binding *b = &expr->binding;
				b; b = b->next) {
			dump_scope_obj(b->object);
			fprintf(stderr, "\n");
			indent(depth + 2);
			fprintf(stderr, "= ");
			dump_expr(b->initializer, depth + 2);
			if (b->next) {
				fprintf(stderr, "\n");
				indent(depth + 1);
			}
		}
		break;
	case EXPR_BREAK:
		fprintf(stderr, "break");
		break;
	case EXPR_CALL:
		fprintf(stderr, "call");
		break;
	case EXPR_CAST:
		switch (expr->cast.kind) {
		case C_CAST:
			fprintf(stderr, "cast ::\n");
			indent(depth + 1);
			dump_expr(expr->cast.value, depth + 1);
			break;
		case C_ASSERTION:
			fprintf(stderr, "type assert :: \n");
			indent(depth + 1);
			dump_expr(expr->cast.value, depth + 1);
			break;
		case C_TEST:
			fprintf(stderr, "test ");
			dump_type(expr->cast.secondary);
			fprintf(stderr, " :: \n");
			indent(depth + 1);
			dump_expr(expr->cast.value, depth + 1);
			break;
		}
		break;
	case EXPR_CONSTANT:
		dump_const(expr);
		break;
	case EXPR_CONTINUE:
		fprintf(stderr, "continue");
		break;
	case EXPR_DEFER:
		fprintf(stderr, "defer");
		break;
	case EXPR_FOR:
		fprintf(stderr, "for");
		break;
	case EXPR_FREE:
		fprintf(stderr, "free");
		break;
	case EXPR_IF:
		fprintf(stderr, "if");
		break;
	case EXPR_LIST:
		fprintf(stderr, "expression list\n");
		indent(depth + 1);
		for (const struct expressions *exprs = &expr->list.exprs;
				exprs; exprs = exprs->next) {
			dump_expr(exprs->expr, depth + 1);
			if (exprs->next) {
				fprintf(stderr, "\n");
				indent(depth + 1);
			}
		}
		break;
	case EXPR_MATCH:
		fprintf(stderr, "match");
		break;
	case EXPR_MEASURE:
		fprintf(stderr, "measure");
		break;
	case EXPR_RETURN:
		fprintf(stderr, "return ");
		dump_expr(expr->_return.value, depth + 1);
		break;
	case EXPR_SLICE:
		fprintf(stderr, "slice");
		break;
	case EXPR_STRUCT:
		fprintf(stderr, "struct");
		break;
	case EXPR_SWITCH:
		fprintf(stderr, "switch");
		break;
	case EXPR_TUPLE:
		fprintf(stderr, "tuple");
		break;
	case EXPR_UNARITHM:
		fprintf(stderr, "unarithm");
		break;
	}
}

static const char *
decl_type_str(enum declaration_type t)
{
	switch (t) {
	case DECL_CONST:
		return "constant";
	case DECL_FUNC:
		return "function";
	case DECL_GLOBAL:
		return "global";
	case DECL_TYPE:
		return "type";
	}
	assert(0);
}

static void
dump_decl(struct declaration *decl)
{
	fprintf(stderr, "%-10s %-20s [export: %d] ",
			decl_type_str(decl->type),
			identifier_unparse(&decl->ident),
			decl->exported);
	const struct type *fntype;
	switch (decl->type) {
	case DECL_CONST:
		dump_type(decl->constant.type);
		fprintf(stderr, " = ");
		dump_const(decl->constant.value);
		break;
	case DECL_FUNC:
		fntype = decl->func.type;
		if (fntype->func.flags & FN_NORETURN) {
			fprintf(stderr, "@noreturn ");
		}
		fprintf(stderr, "(");
		for (struct type_func_param *param = fntype->func.params;
				param; param = param->next) {
			dump_type(param->type);
			if (param->next) {
				fprintf(stderr, ", ");
			}
		}
		fprintf(stderr, ") ");
		dump_type(fntype->func.result);
		fprintf(stderr, " = ");
		dump_expr(decl->func.body, 0);
		break;
	case DECL_GLOBAL:
		dump_type(decl->global.type);
		fprintf(stderr, " = ");
		dump_const(decl->global.value);
		break;
	case DECL_TYPE:
		dump_type(decl->_type);
		break;
	}
	fprintf(stderr, "\n");
}

void
dump_unit(struct unit *unit)
{
	for (struct declarations *d = unit->declarations; d; d = d->next) {
		dump_decl(d->decl);
	}
}
