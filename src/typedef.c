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
	const struct type *t = type_dealias(NULL, expr->result);
	switch (t->storage) {
	case STORAGE_BOOL:
		xfprintf(out, "%s", val->bval ? "true" : "false");
		break;
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:;
		const char *suffix = storage_to_suffix(expr->result->storage);
		if (isnan(val->fval)) {
			xfprintf(out, "0.0%s / 0.0%s", suffix, suffix);
		} else if (isinf(val->fval)) {
			xfprintf(out, "%s1.0%s / 0.0%s",
				(val->fval > 0) ? "" : "-", suffix, suffix);
		} else {
			xfprintf(out, "%a%s", val->fval, suffix);
		}
		break;
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_ICONST:
	case STORAGE_INT:
		xfprintf(out, "%" PRIiMAX "%s", val->ival,
			storage_to_suffix(type_dealias(NULL, expr->result)->storage));
		break;
	case STORAGE_NULL:
		xfprintf(out, "null");
		break;
	case STORAGE_SIZE:
	case STORAGE_U16:
	case STORAGE_U32:
	case STORAGE_U64:
	case STORAGE_U8:
	case STORAGE_UINT:
	case STORAGE_UINTPTR:
		xfprintf(out, "%" PRIuMAX "%s", val->uval,
			storage_to_suffix(type_dealias(NULL, expr->result)->storage));
		break;
	case STORAGE_VOID:
		xfprintf(out, "void");
		break;
	case STORAGE_RCONST:
	case STORAGE_RUNE:
		xfprintf(out, "\'\\U%08" PRIx32 "\'", (uint32_t)val->uval);
		break;
	case STORAGE_STRING:
		xfprintf(out, "\"");
		for (size_t i = 0; i < val->string.len; i += 1) {
			char c = val->string.value[i];
			if (isalnum((unsigned char)c)) {
				xfprintf(out, "%c", c);
			} else {
				xfprintf(out, "\\x%02X", c);
			}
		};
		xfprintf(out, "\"");
		break;
	case STORAGE_ENUM:;
		char *ident = identifier_unparse(&expr->result->alias.ident);
		if (t->alias.type->storage == STORAGE_UINTPTR) {
			xfprintf(out, "%" PRIuMAX ": uintptr", val->uval);
		} else if (type_is_signed(NULL, t->alias.type)) {
			xfprintf(out, "%" PRIiMAX "%s: %s", val->ival,
				storage_to_suffix(t->alias.type->storage), ident);
		} else {
			xfprintf(out, "%" PRIuMAX "%s: %s", val->uval,
				storage_to_suffix(t->alias.type->storage), ident);
		}
		free(ident);
		break;
	case STORAGE_TAGGED:
		emit_const(expr->constant.tagged.value, out);
		xfprintf(out, ": ");
		emit_type(expr->constant.tagged.tag, out);
		break;
	case STORAGE_ARRAY:
		xfprintf(out, "[");
		for (const struct array_constant *item = val->array;
				item; item = item->next) {
			emit_const(item->value, out);
			if (item->next) {
				xfprintf(out, ", ");
			}
		}
		if (t->array.expandable) {
			xfprintf(out, "...");
		}
		xfprintf(out, "]");
		break;
	case STORAGE_TUPLE:
		xfprintf(out, "(");
		for (const struct tuple_constant *item = val->tuple;
				item; item = item->next) {
			emit_const(item->value, out);
			if (item->next) {
				xfprintf(out, ", ");
			}
		}
		xfprintf(out, ")");
		break;
	case STORAGE_SLICE:
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		assert(0); // TODO
	case STORAGE_ALIAS:
	case STORAGE_ERROR:
	case STORAGE_FUNCTION:
	case STORAGE_NEVER:
	case STORAGE_OPAQUE:
	case STORAGE_POINTER:
	case STORAGE_VALIST:
		assert(0); // Invariant
	}
}

static void
emit_struct(const struct type *type, FILE *out)
{
	xfprintf(out, "%s %s{ ",
			type->storage == STORAGE_STRUCT ? "struct" : "union",
			type->struct_union.packed ? "@packed " : "");
	for (const struct struct_field *f = type->struct_union.fields;
			f; f = f->next) {
		if (!type->struct_union.c_compat) {
			xfprintf(out, "@offset(%zu) ", f->offset);
		}
		if (f->name) {
			xfprintf(out, "%s: ", f->name);
		}
		emit_type(f->type, out);
		xfprintf(out, ", ");
	}
	xfprintf(out, "}");
}

void
emit_type(const struct type *type, FILE *out)
{
	if (type->flags & TYPE_CONST) {
		xfprintf(out, "const ");
	}
	if (type->flags & TYPE_ERROR) {
		xfprintf(out, "!");
	}

	char *ident;
	switch (type->storage) {
	case STORAGE_BOOL:
	case STORAGE_ERROR:
	case STORAGE_F32:
	case STORAGE_F64:
	case STORAGE_FCONST:
	case STORAGE_I16:
	case STORAGE_I32:
	case STORAGE_I64:
	case STORAGE_I8:
	case STORAGE_INT:
	case STORAGE_NEVER:
	case STORAGE_NULL:
	case STORAGE_OPAQUE:
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
		xfprintf(out, "%s", type_storage_unparse(type->storage));
		break;
	case STORAGE_POINTER:
		xfprintf(out, "%s*", type->pointer.flags & PTR_NULLABLE
				? "nullable " : "");
		emit_type(type->pointer.referent, out);
		break;
	case STORAGE_ARRAY:
		if (type->array.length == SIZE_UNDEFINED) {
			xfprintf(out, "[*]");
		} else {
			xfprintf(out, "[%zu]", type->array.length);
		}
		emit_type(type->array.members, out);
		break;
	case STORAGE_SLICE:
		xfprintf(out, "[]");
		emit_type(type->array.members, out);
		break;
	case STORAGE_ALIAS:
		ident = identifier_unparse(&type->alias.ident);
		xfprintf(out, "%s", ident);
		free(ident);
		break;
	case STORAGE_TAGGED:
		xfprintf(out, "(");
		for (const struct type_tagged_union *tu = &type->tagged;
				tu; tu = tu->next) {
			emit_type(tu->type, out);
			if (tu->next) {
				xfprintf(out, " | ");
			}
		}
		xfprintf(out, ")");
		break;
	case STORAGE_STRUCT:
	case STORAGE_UNION:
		emit_struct(type, out);
		break;
	case STORAGE_FUNCTION:
		xfprintf(out, "fn(");
		for (const struct type_func_param *param = type->func.params;
				param; param = param->next) {
			if (param->next) {
				emit_type(param->type, out);
				xfprintf(out, ", ");
			} else if (type->func.variadism == VARIADISM_HARE) {
				emit_type(param->type->array.members, out);
				xfprintf(out, "...");
			} else if (type->func.variadism == VARIADISM_C) {
				emit_type(param->type, out);
				xfprintf(out, ", ...");
			} else {
				emit_type(param->type, out);
			}
		}
		xfprintf(out, ") ");
		emit_type(type->func.result, out);
		break;
	case STORAGE_ENUM:
		ident = identifier_unparse(&type->alias.ident);
		xfprintf(out, "%s", ident);
		free(ident);
		break;
	case STORAGE_TUPLE:
		xfprintf(out, "(");
		for (const struct type_tuple *tuple = &type->tuple;
				tuple; tuple = tuple->next) {
			emit_type(tuple->type, out);
			if (tuple->next) {
				xfprintf(out, ", ");
			}
		}
		xfprintf(out, ")");
		break;
	case STORAGE_ICONST:
		xfprintf(out, "[iconst min=%jd max=%jd]", type->_const.min,
			type->_const.max);
		break;
	}
}

static void
emit_decl_const(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	xfprintf(out, "export def %s", ident);
	assert(decl->constant.type);
	if (decl->constant.type->size != SIZE_UNDEFINED) {
		xfprintf(out, ": ");
		emit_type(decl->constant.type, out);
	}
	free(ident);
	xfprintf(out, " = ");
	emit_const(decl->constant.value, out);
	xfprintf(out, ";\n");
}

static void
emit_decl_func(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	const struct type *fntype = decl->func.type;
	xfprintf(out, "export ");
	if (decl->symbol) {
		xfprintf(out, "@symbol(\"%s\") ", decl->symbol);
	}
	xfprintf(out, "fn %s(", ident);

	for (struct type_func_param *param = fntype->func.params;
			param; param = param->next) {
		if (param->next) {
			emit_type(param->type, out);
			xfprintf(out, ", ");
		} else if (fntype->func.variadism == VARIADISM_HARE) {
			emit_type(param->type->array.members, out);
			xfprintf(out, "...");
		} else if (fntype->func.variadism == VARIADISM_C) {
			emit_type(param->type, out);
			xfprintf(out, ", ...");
		} else {
			emit_type(param->type, out);
		}
	}

	xfprintf(out, ") ");
	emit_type(fntype->func.result, out);
	xfprintf(out, ";\n");
	free(ident);
}

static void
emit_decl_global(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	xfprintf(out, "export let ");
	if (decl->symbol) {
		xfprintf(out, "@symbol(\"%s\") ", decl->symbol);
	}
	if (decl->global.threadlocal) {
		xfprintf(out, "@threadlocal ");
	}
	xfprintf(out, "%s: ", ident);
	if (decl->constant.type) {
		emit_type(decl->global.type, out);
	} else {
		emit_type(decl->global.value->result, out);
	};
	xfprintf(out, ";\n");
	free(ident);
}

static void
emit_decl_type(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident);
	xfprintf(out, "export type %s = ", ident);
	assert(decl->type->storage == STORAGE_ALIAS
			|| decl->type->storage == STORAGE_ENUM);
	if (decl->type->storage == STORAGE_ENUM) {
		const struct type *type = decl->type;
		xfprintf(out, "enum %s { ",
			type_storage_unparse(type->alias.type->storage));
		for (const struct scope_object *ev = type->_enum.values->objects;
				ev; ev = ev->lnext) {
			assert(ev->otype != O_SCAN);
			xfprintf(out, "%s = ", ev->name.name);
			emit_const(ev->value, out);
			xfprintf(out, ", ");
		}
		xfprintf(out, "}");
	} else {
		emit_type(decl->type->alias.type, out);
	}
	xfprintf(out, "; // size: ");
	if (decl->type->size == SIZE_UNDEFINED) {
		xfprintf(out, "undefined");
	} else {
		xfprintf(out, "%zu", decl->type->size);
	}
	xfprintf(out, ", align: ");
	if (decl->type->align == ALIGN_UNDEFINED) {
		xfprintf(out, "undefined");
	} else {
		xfprintf(out, "%zu", decl->type->align);
	}
	xfprintf(out, ", id: %u\n", decl->type->id);
	free(ident);
}

void
emit_typedefs(struct unit *unit, FILE *out)
{
	for (struct identifiers *imports = unit->imports;
			imports; imports = imports->next) {
		char *ident = identifier_unparse(&imports->ident);
		xfprintf(out, "use %s;\n", ident);
		free(ident);
	}

	for (struct declarations *decls = unit->declarations;
			decls; decls = decls->next) {
		struct declaration *decl = &decls->decl;
		if (!decl->exported) {
			continue;
		}

		switch (decl->decl_type) {
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
