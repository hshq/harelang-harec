#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <math.h>
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
	case STORAGE_CHAR:
		return "";
	case STORAGE_F32:
		return "f32";
	case STORAGE_F64:
		return "f64";
	case STORAGE_FCONST:
		return "";
	case STORAGE_I16:
		return "i16";
	case STORAGE_I32:
		return "i32";
	case STORAGE_I64:
		return "i64";
	case STORAGE_I8:
		return "i8";
	case STORAGE_ICONST:
		return "";
	case STORAGE_INT:
		return "i";
	case STORAGE_SIZE:
		return "z";
	case STORAGE_U16:
		return "u16";
	case STORAGE_U32:
		return "u32";
	case STORAGE_U64:
		return "u64";
	case STORAGE_U8:
		return "u8";
	case STORAGE_UINT:
		return "u";
	case STORAGE_UINTPTR:
		return "u64: uintptr";
	case STORAGE_VALIST:
		return "valist";
	default:
		assert(0);
	}
}

static void
emit_const(const struct expression *expr, FILE *out)
{
	assert(expr->type == EXPR_CONSTANT);
	const struct expression_constant *val = &expr->constant;
	assert(!val->object);
	switch (type_dealias(expr->result)->storage) {
	case STORAGE_BOOL:
		fprintf(out, "%s", val->bval ? "false" : "true");
		break;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	{
		const char *suffix = storage_to_suffix(expr->result->storage);
		if (isnan(val->fval)) {
			fprintf(out, "0.0%s / 0.0%s", suffix, suffix);
		} else if (isinf(val->fval)) {
			fprintf(out, "%s1.0%s / 0.0%s",
				(val->fval > 0) ? "" : "-", suffix, suffix);
		} else {
			fprintf(out, "%lf%s", val->fval, suffix);
		}
		break;
	}
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_ICONST:
	case STORAGE_INT:
		fprintf(out, "%" PRIiMAX "%s", val->ival,
			storage_to_suffix(type_dealias(expr->result)->storage));
		break;
	case STORAGE_NULL:
		fprintf(out, "null");
		break;
	case STORAGE_SIZE:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
		fprintf(out, "%" PRIuMAX "%s", val->uval,
			storage_to_suffix(type_dealias(expr->result)->storage));
		break;
	case STORAGE_VOID:
		fprintf(out, "void");
		break;
	case STORAGE_RCONST:
	case STORAGE_RUNE:
		fprintf(out, "\'\\U%08" PRIx32 "\'", (uint32_t)val->uval);
		break;
	case STORAGE_STRING:
		fprintf(out, "\"");
		for (size_t i = 0; i < val->string.len; i += 1) {
			char c = val->string.value[i];
			if (isalnum((unsigned char)c)) {
				fprintf(out, "%c", c);
			} else {
				fprintf(out, "\\x%02X", c);
			}
		};
		fprintf(out, "\"");
		break;
	case STORAGE_ENUM:
		assert(expr->result->storage == STORAGE_ENUM);
		const struct type *t = type_dealias(expr->result);
		char *ident = identifier_unparse(&t->alias.ident);
		if (t->alias.type->storage == STORAGE_CHAR) {
			fprintf(out, "%" PRIuMAX, val->uval);
		} else if (type_is_signed(t->alias.type)) {
			fprintf(out, "%" PRIiMAX "%s: %s", val->ival,
				storage_to_suffix(t->alias.type->storage), ident);
		} else {
			fprintf(out, "%" PRIuMAX "%s: %s", val->uval,
				storage_to_suffix(t->alias.type->storage), ident);
		}
		free(ident);
		break;
	case STORAGE_ALIAS:
	case STORAGE_ARRAY:
	case STORAGE_SLICE:
	case STORAGE_STRUCT:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
		assert(0); // TODO
	case STORAGE_CHAR:
	case STORAGE_FUNCTION:
	case STORAGE_POINTER:
	case STORAGE_TAGGED:
	case STORAGE_VALIST:
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

static bool
emit_struct(const struct type *type, FILE *out)
{
	bool ret = true;
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

	fprintf(out, "%s { ", type->storage == STORAGE_STRUCT
			? "struct" : "union");
	for (size_t i = 0; i < n; ++i) {
		const struct struct_field *f = fields[i];
		if (!type->struct_union.c_compat) {
			fprintf(out, "@offset(%zd) ", f->offset);
		}
		if (f->name) {
			fprintf(out, "%s: ", f->name);
		}
		ret &= emit_type(f->type, out);
		fprintf(out, ", ");
	}
	fprintf(out, "}");
	return ret;
}

bool
emit_type(const struct type *type, FILE *out)
{
	bool ret = true;
	if (type->flags & TYPE_CONST) {
		fprintf(out, "const ");
	}
	if (type->flags & TYPE_ERROR) {
		fprintf(out, "!");
	}

	char *ident;
	switch (type->storage) {
	case STORAGE_BOOL:
	case STORAGE_CHAR:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_INT:
	case STORAGE_NULL:
	case STORAGE_RCONST:
	case STORAGE_RUNE:
	case STORAGE_SIZE:
	case STORAGE_STRING:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
	case STORAGE_VALIST:
	case STORAGE_VOID:
		fprintf(out, "%s", type_storage_unparse(type->storage));
		break;
	case STORAGE_POINTER:
		fprintf(out, "%s*", type->pointer.flags & PTR_NULLABLE
				? "nullable " : "");
		ret &= emit_type(type->pointer.referent, out);
		break;
	case STORAGE_ARRAY:
		if (type->array.length == SIZE_UNDEFINED) {
			fprintf(out, "[*]");
		} else {
			fprintf(out, "[%zd]", type->array.length);
		}
		ret &= emit_type(type->array.members, out);
		break;
	case STORAGE_SLICE:
		fprintf(out, "[]");
		ret &= emit_type(type->array.members, out);
		break;
	case STORAGE_ALIAS:
		ret &= type->alias.exported;
		ident = identifier_unparse(&type->alias.ident);
		fprintf(out, "%s", ident);
		free(ident);
		break;
	case STORAGE_TAGGED:
		fprintf(out, "(");
		for (const struct type_tagged_union *tu = &type->tagged;
				tu; tu = tu->next) {
			ret &= emit_type(tu->type, out);
			if (tu->next) {
				fprintf(out, " | ");
			}
		}
		fprintf(out, ")");
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		ret &= emit_struct(type, out);
		break;
	case STORAGE_FUNCTION:
		if (type->func.flags & FN_NORETURN) {
			fprintf(out, "@noreturn ");
		}
		fprintf(out, "fn(");
		for (const struct type_func_param *param = type->func.params;
				param; param = param->next) {
			fprintf(out, "_: ");
			if (param->next) {
				ret &= emit_type(param->type, out);
				fprintf(out, ", ");
			} else if (type->func.variadism == VARIADISM_HARE) {
				ret &= emit_type(param->type->array.members, out);
				fprintf(out, "...");
			} else if (type->func.variadism == VARIADISM_C) {
				ret &= emit_type(param->type, out);
				fprintf(out, ", ...");
			} else {
				ret &= emit_type(param->type, out);
			}
		}
		fprintf(out, ") ");
		ret &= emit_type(type->func.result, out);
		break;
	case STORAGE_ENUM:
		ret &= type->alias.exported;
		ident = identifier_unparse(&type->alias.ident);
		fprintf(out, "%s", ident);
		free(ident);
		break;
	case STORAGE_TUPLE:
		fprintf(out, "(");
		for (const struct type_tuple *tuple = &type->tuple;
				tuple; tuple = tuple->next) {
			ret &= emit_type(tuple->type, out);
			if (tuple->next) {
				fprintf(out, ", ");
			}
		}
		fprintf(out, ")");
		break;
	case STORAGE_ICONST:
		fprintf(out, "[iconst min=%jd max=%jd]", type->_const.min,
			type->_const.max);
		break;
	}
	return ret;
}

static void
emit_exported_type(const struct type *type, FILE *out)
{
	if (!emit_type(type, out)) {
		// XXX: Hack
		struct type *_type = (struct type *)type;
		_type->alias.exported = true;
		fprintf(stderr, "Cannot use unexported type ");
		emit_type(type, stderr);
		fprintf(stderr, " in exported declaration\n");
		exit(EXIT_FAILURE);
	}
}

static void
emit_decl_const(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	fprintf(out, "export def %s: ", ident);
	free(ident);
	emit_exported_type(decl->constant.type, out);
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
		fprintf(out, "_: ");
		if (param->next) {
			emit_exported_type(param->type, out);
			fprintf(out, ", ");
		} else if (fntype->func.variadism == VARIADISM_HARE) {
			emit_exported_type(param->type->array.members, out);
			fprintf(out, "...");
		} else if (fntype->func.variadism == VARIADISM_C) {
			emit_exported_type(param->type, out);
			fprintf(out, ", ...");
		} else {
			emit_exported_type(param->type, out);
		}
	}

	fprintf(out, ") ");
	emit_exported_type(fntype->func.result, out);
	fprintf(out, ";\n");
	free(ident);
}

static void
emit_decl_global(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	fprintf(out, "export let ");
	if (decl->symbol) {
		fprintf(out, "@symbol(\"%s\") ", decl->symbol);
	}
	fprintf(out, "%s: ", ident);
	emit_exported_type(decl->global.type, out);
	fprintf(out, ";\n");
}

static void
emit_decl_type(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	fprintf(out, "export type %s = ", ident);
	assert(decl->_type->storage == STORAGE_ALIAS
			|| decl->_type->storage == STORAGE_ENUM);
	if (decl->_type->storage == STORAGE_ENUM) {
		const struct type *type = decl->_type;
		fprintf(out, "enum %s { ",
			type_storage_unparse(type->alias.type->storage));
		for (const struct scope_object *ev = type->_enum.values->objects;
				ev; ev = ev->lnext) {
			if (ev->otype == O_SCAN) {
				continue;
			}
			fprintf(out, "%s = ", ev->name.name);
			emit_const(ev->value, out);
			fprintf(out, ", ");
		}
		fprintf(out, "}");
	} else {
		emit_exported_type(decl->_type->alias.type, out);
	}
	fprintf(out, "; // size: %zd, align: %zd, id: %u\n",
		decl->_type->size, decl->_type->align, decl->_type->id);
}

void
emit_typedefs(struct unit *unit, FILE *out)
{
	for (struct identifiers *imports = unit->imports;
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
