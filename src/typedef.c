#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "check.h"
#include "expr.h"
#include "identifier.h"
#include "typedef.h"
#include "util.h"

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
emit_const(const struct expression *expr, FILE *out)
{
	assert(expr->type == EXPR_CONSTANT);
	const union expression_constant *val = &expr->constant;
	switch (expr->result->storage) {
	case TYPE_STORAGE_BOOL:
		fprintf(out, "%s", val->bval ? "false" : "true");
		break;
	case TYPE_STORAGE_F32:
	case TYPE_STORAGE_F64:
		fprintf(out, "%lf%s", val->fval,
			storage_to_suffix(expr->result->storage));
		break;
	case TYPE_STORAGE_I16:
	case TYPE_STORAGE_I32:
	case TYPE_STORAGE_I64:
	case TYPE_STORAGE_I8:
	case TYPE_STORAGE_INT:
		fprintf(out, "%ld%s", val->ival,
			storage_to_suffix(expr->result->storage));
		break;
	case TYPE_STORAGE_NULL:
		fprintf(out, "null");
		break;
	case TYPE_STORAGE_SIZE:
	case TYPE_STORAGE_U16:
	case TYPE_STORAGE_U32:
	case TYPE_STORAGE_U64:
	case TYPE_STORAGE_U8:
	case TYPE_STORAGE_UINT:
	case TYPE_STORAGE_UINTPTR:
		fprintf(out, "%lu%s", val->uval,
			storage_to_suffix(expr->result->storage));
		break;
	case TYPE_STORAGE_VOID:
		fprintf(out, "void");
		break;
	case TYPE_STORAGE_RUNE:
		assert(0); // TODO
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_ENUM:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	case TYPE_STORAGE_CHAR:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_TAGGED:
		assert(0); // Invariant
	}
}

static int
field_compar(const void *_a, const void *_b)
{
	const struct struct_field **a = (const struct struct_field **)_a;
	const struct struct_field **b = (const struct struct_field **)_b;
	return (*a)->offset - (*b)->offset;
}

static void
emit_struct(const struct type *type, FILE *out)
{
	// TODO: This can be greatly simplified when we have explicit field
	// offsets for structs.
	size_t n = 0;
	for (const struct struct_field *f = type->struct_union.fields;
			f; f = f->next) {
		++n;
	}
	const struct struct_field **fields = xcalloc(
		sizeof(const struct struct_field *), n);
	n = 0;
	for (const struct struct_field *f = type->struct_union.fields;
			f; f = f->next) {
		fields[n++] = f;
	}

	qsort(fields, n, sizeof(fields[0]), field_compar);

	assert(type->struct_union.c_compat); // TODO
	fprintf(out, "%s { ", type->storage == TYPE_STORAGE_STRUCT
			? "struct" : "union");
	for (size_t i = 0; i < n; ++i) {
		const struct struct_field *f = fields[i]; 
		fprintf(out, "%s: ", f->name);
		emit_type(f->type, out);
		fprintf(out, ", ");
	}
	fprintf(out, "}");
}

void
emit_type(const struct type *type, FILE *out)
{
	if (type->flags & TYPE_CONST) {
		fprintf(out, "const ");
	}

	char *ident;
	switch (type->storage) {
	case TYPE_STORAGE_BOOL:
	case TYPE_STORAGE_CHAR:
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
		fprintf(out, "%s", type_storage_unparse(type->storage));
		break;
	case TYPE_STORAGE_POINTER:
		fprintf(out, "%s*", type->pointer.flags & PTR_NULLABLE
				? "nullable " : "");
		emit_type(type->pointer.referent, out);
		break;
	case TYPE_STORAGE_ARRAY:
		if (type->array.length == SIZE_UNDEFINED) {
			fprintf(out, "[*]");
		} else {
			fprintf(out, "[%zd]", type->array.length);
		}
		emit_type(type->array.members, out);
		break;
	case TYPE_STORAGE_SLICE:
		fprintf(out, "[]");
		emit_type(type->array.members, out);
		break;
	case TYPE_STORAGE_ALIAS:
		ident = identifier_unparse(&type->alias.ident);
		fprintf(out, "%s", ident);
		free(ident);
		break;
	case TYPE_STORAGE_TAGGED:
		fprintf(out, "(");
		for (const struct type_tagged_union *tu = &type->tagged;
				tu; tu = tu->next) {
			emit_type(tu->type, out);
			if (tu->next) {
				fprintf(out, " | ");
			}
		}
		fprintf(out, ")");
		break;
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_UNION:
		emit_struct(type, out);
		break;
	case TYPE_STORAGE_FUNCTION:
		if (type->func.flags & FN_NORETURN) {
			fprintf(out, "@noreturn ");
		}
		fprintf(out, "fn(");
		for (const struct type_func_param *param = type->func.params;
				param; param = param->next) {
			if (param->next) {
				emit_type(param->type, out);
				fprintf(out, ", ");
			} else if (type->func.variadism == VARIADISM_HARE) {
				emit_type(param->type->array.members, out);
				fprintf(out, "...");
			} else if (type->func.variadism == VARIADISM_C) {
				emit_type(param->type, out);
				fprintf(out, ", ...");
			} else {
				emit_type(param->type, out);
			}
		}
		fprintf(out, ") ");
		emit_type(type->func.result, out);
		break;
	case TYPE_STORAGE_ENUM:
		fprintf(out, "enum %s { ", type_storage_unparse(type->_enum.storage));
		for (const struct type_enum_value *ev = type->_enum.values;
				ev; ev = ev->next) {
			fprintf(out, "%s = ", ev->name);
			if (type_is_signed(type)) {
				fprintf(out, "%zu%s", ev->ival,
					storage_to_suffix(type->_enum.storage));
			} else {
				fprintf(out, "%zd%s", ev->uval,
					storage_to_suffix(type->_enum.storage));
			}
			if (ev->next) {
				fprintf(out, ", ");
			}
		}
		fprintf(out, "}");
		break;
	}
}

static void
emit_decl_const(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	fprintf(out, "export def %s: ", ident);
	free(ident);
	emit_type(decl->constant.type, out);
	fprintf(out, " = ");
	emit_const(decl->constant.value, out);
	fprintf(out, ";\n");
}

static void
emit_decl_func(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	const struct type *fntype = decl->func.type;
	fprintf(out, "export");
	if (decl->symbol) {
		fprintf(out, " @symbol(\"%s\")", decl->symbol);
	}
	fprintf(out, "%s fn %s(",
		(fntype->func.flags & FN_NORETURN) ? " @noreturn" : "",
		ident);

	for (struct type_func_param *param = fntype->func.params;
			param; param = param->next) {
		if (param->next) {
			emit_type(param->type, out);
			fprintf(out, ", ");
		} else if (fntype->func.variadism == VARIADISM_HARE) {
			emit_type(param->type->array.members, out);
			fprintf(out, "...");
		} else if (fntype->func.variadism == VARIADISM_C) {
			emit_type(param->type, out);
			fprintf(out, ", ...");
		} else {
			emit_type(param->type, out);
		}
	}

	fprintf(out, ") ");
	emit_type(fntype->func.result, out);
	fprintf(out, ";\n");
	free(ident);
}

static void
emit_decl_global(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	fprintf(out, "export");
	if (decl->symbol) {
		fprintf(out, " @symbol(\"%s\") ", decl->symbol);
	}
	fprintf(out, " let %s: ", ident);
	emit_type(decl->global.type, out);
	fprintf(out, ";\n");
}

static void
emit_decl_type(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	fprintf(out, "export type %s = ", ident);
	emit_type(decl->_type, out);
	fprintf(out, ";\n");
}

void
emit_typedefs(struct unit *unit, FILE *out)
{
	for (struct imports *imports = unit->imports;
			imports; imports = imports->next) {
		char *ident = identifier_unparse(&imports->ident);
		fprintf(out, "use %s;\n", ident);
		free(ident);
	}

	for (struct declarations *decls = unit->declarations;
			decls; decls = decls->next) {
		struct declaration *decl = decls->decl;
		if (!decl->exported) {
			continue;
		}

		switch (decl->type) {
		case DECL_CONST:
			emit_decl_const(decl, out);
			break;
		case DECL_FUNC:
			emit_decl_func(decl, out);
			break;
		case DECL_GLOBAL:
			emit_decl_global(decl, out);
			break;
		case DECL_TYPE:
			emit_decl_type(decl, out);
			break;
		}
	}
}
