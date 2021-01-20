#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "check.h"
#include "identifier.h"
#include "typedef.h"

static void
emit_type(const struct type *type, FILE *out)
{
	if (type->flags & TYPE_CONST) {
		fprintf(out, "const ");
	}

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
		fprintf(out, "%s", type_storage_unparse(type->storage));
		break;
	case TYPE_STORAGE_ALIAS:
	case TYPE_STORAGE_ARRAY:
	case TYPE_STORAGE_FUNCTION:
	case TYPE_STORAGE_POINTER:
	case TYPE_STORAGE_SLICE:
	case TYPE_STORAGE_STRING:
	case TYPE_STORAGE_STRUCT:
	case TYPE_STORAGE_TAGGED_UNION:
	case TYPE_STORAGE_UNION:
		assert(0); // TODO
	}
}

static void
emit_func(struct declaration *decl, FILE *out)
{
	char *ident = identifier_unparse(&decl->ident); // TODO: Emit @symbol

	const struct type *fntype = decl->func.type;
	fprintf(out, "export%s fn %s(",
		(fntype->func.flags & FN_NORETURN) ? " @noreturn" : "",
		ident);

	for (struct type_func_param *param = fntype->func.params;
			param; param = param->next) {
		emit_type(param->type, out);
		if (param->next) {
			fprintf(out, ", ");
		}
	}

	fprintf(out, ") ");
	emit_type(fntype->func.result, out);
	fprintf(out, ";\n");
	free(ident);
}

void
emit_typedefs(struct unit *unit, FILE *out)
{
	for (struct declarations *decls = unit->declarations;
			decls; decls = decls->next) {
		struct declaration *decl = decls->decl;
		if (!decl->exported) {
			continue;
		}

		switch (decl->type) {
		case DECL_FUNC:
			emit_func(decl, out);
			break;
		case DECL_TYPE:
			assert(0); // TODO
		case DECL_GLOBAL:
			assert(0); // TODO
		case DECL_CONSTANT:
			assert(0); // TODO
		}
	}
}
