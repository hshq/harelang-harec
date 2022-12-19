#include <assert.h>
#include <stdlib.h>
#include "gen.h"
#include "qbe.h"
#include "types.h"
#include "type_store.h"

enum qbe_instr
alloc_for_align(size_t align)
{
	switch (align) {
	case 1:
	case 2:
	case 4:
		return Q_ALLOC4;
	case 8:
		return Q_ALLOC8;
	case 16:
		return Q_ALLOC16;
	default:
		abort();
	}
}

enum qbe_instr
store_for_type(struct gen_context *ctx, const struct type *type)
{
	switch (type->storage) {
	case STORAGE_CHAR:
	case STORAGE_I8:
	case STORAGE_U8:
	case STORAGE_BOOL:
		return Q_STOREB;
	case STORAGE_I16:
	case STORAGE_U16:
		return Q_STOREH;
	case STORAGE_I32:
	case STORAGE_U32:
	case STORAGE_INT:
	case STORAGE_UINT:
	case STORAGE_RCONST:
	case STORAGE_RUNE:
		return Q_STOREW;
	case STORAGE_I64:
	case STORAGE_U64:
		return Q_STOREL;
	case STORAGE_F32:
		return Q_STORES;
	case STORAGE_F64:
		return Q_STORED;
	case STORAGE_SIZE:
		switch (ctx->arch.sz->stype) {
		case Q_LONG:
			return Q_STOREL;
		default:
			assert(0);
		}
		break;
	case STORAGE_POINTER:
	case STORAGE_UINTPTR:
		switch (ctx->arch.ptr->stype) {
		case Q_LONG:
			return Q_STOREL;
		default:
			assert(0);
		}
		break;
	case STORAGE_ENUM:
	case STORAGE_ALIAS:
		return store_for_type(ctx, type->alias.type);
	case STORAGE_ARRAY:
	case STORAGE_ERROR:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_NULL:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
	case STORAGE_VOID:
		abort(); // Invariant
	}
	abort(); // Unreachable
}

enum qbe_instr
load_for_type(struct gen_context *ctx, const struct type *type)
{
	switch (type->storage) {
	case STORAGE_I8:
		return Q_LOADSB;
	case STORAGE_CHAR:
	case STORAGE_U8:
	case STORAGE_BOOL:
		return Q_LOADUB;
	case STORAGE_I16:
		return Q_LOADSH;
	case STORAGE_U16:
		return Q_LOADUH;
	case STORAGE_U32:
	case STORAGE_UINT:
	case STORAGE_RCONST:
	case STORAGE_RUNE:
		return Q_LOADUW;
	case STORAGE_I32:
	case STORAGE_INT:
		return Q_LOADSW;
	case STORAGE_I64:
	case STORAGE_U64:
		return Q_LOADL;
	case STORAGE_F32:
		return Q_LOADS;
	case STORAGE_F64:
		return Q_LOADD;
	case STORAGE_SIZE:
		switch (ctx->arch.sz->stype) {
		case Q_LONG:
			return Q_LOADL;
		default:
			assert(0);
		}
		break;
	case STORAGE_POINTER:
	case STORAGE_UINTPTR:
		switch (ctx->arch.ptr->stype) {
		case Q_LONG:
			return Q_LOADL;
		default:
			assert(0);
		}
		break;
	case STORAGE_ENUM:
	case STORAGE_ALIAS:
		return load_for_type(ctx, type->alias.type);
	case STORAGE_ARRAY:
	case STORAGE_ERROR:
	case STORAGE_FCONST:
	case STORAGE_FUNCTION:
	case STORAGE_ICONST:
	case STORAGE_NULL:
	case STORAGE_SLICE:
	case STORAGE_STRING:
	case STORAGE_STRUCT:
	case STORAGE_TAGGED:
	case STORAGE_TUPLE:
	case STORAGE_UNION:
	case STORAGE_VALIST:
	case STORAGE_VOID:
		abort(); // Invariant
	}
	abort(); // Unreachable
}

enum qbe_instr
binarithm_for_op(struct gen_context *ctx,
		enum binarithm_operator op,
		const struct type *type)
{
	// TODO: NaN, udiv et al
	bool is_signed = type_is_signed(type);
	enum qbe_stype stype = qtype_lookup(ctx, type, false)->stype;
	assert(stype != Q__AGGREGATE && stype != Q__VOID);
	switch (op) {
	case BIN_PLUS:
		return Q_ADD;
	case BIN_BAND:
		return Q_AND;
	case BIN_DIV:
		return is_signed ? Q_DIV : Q_UDIV;
	case BIN_MINUS:
		return Q_SUB;
	case BIN_TIMES:
		return Q_MUL;
	case BIN_MODULO:
		return is_signed ? Q_REM : Q_UREM;
	case BIN_BOR:
		return Q_OR;
	case BIN_BXOR:
		return Q_XOR;
	case BIN_LSHIFT:
		return Q_SHL;
	case BIN_RSHIFT:
		return is_signed ? Q_SAR : Q_SHR;
	case BIN_LEQUAL:
		switch (stype) {
		case Q_WORD:
			return Q_CEQW;
		case Q_LONG:
			return Q_CEQL;
		case Q_SINGLE:
			return Q_CEQS;
		case Q_DOUBLE:
			return Q_CEQD;
		default:
			assert(0);
		}
		break;
	case BIN_NEQUAL:
	case BIN_LXOR:
		switch (stype) {
		case Q_WORD:
			return Q_CNEW;
		case Q_LONG:
			return Q_CNEL;
		case Q_SINGLE:
			return Q_CNES;
		case Q_DOUBLE:
			return Q_CNED;
		default:
			assert(0);
		}
		break;
	case BIN_GREATER:
		switch (stype) {
		case Q_WORD:
			return is_signed ? Q_CSGTW : Q_CUGTW;
		case Q_LONG:
			return is_signed ? Q_CSGTL : Q_CUGTL;
		case Q_SINGLE:
			return Q_CGTS;
		case Q_DOUBLE:
			return Q_CGTD;
		default:
			assert(0);
		}
	case BIN_GREATEREQ:
		switch (stype) {
		case Q_WORD:
			return is_signed ? Q_CSGEW : Q_CUGEW;
		case Q_LONG:
			return is_signed ? Q_CSGEL : Q_CUGEL;
		case Q_SINGLE:
			return Q_CGES;
		case Q_DOUBLE:
			return Q_CGED;
		default:
			assert(0);
		}
		break;
	case BIN_LESS:
		switch (stype) {
		case Q_WORD:
			return is_signed ? Q_CSLTW : Q_CULTW;
		case Q_LONG:
			return is_signed ? Q_CSLTL : Q_CULTL;
		case Q_SINGLE:
			return Q_CLTS;
		case Q_DOUBLE:
			return Q_CLTD;
		default:
			assert(0);
		}
		break;
	case BIN_LESSEQ:
		switch (stype) {
		case Q_WORD:
			return is_signed ? Q_CSLEW : Q_CULEW;
		case Q_LONG:
			return is_signed ? Q_CSLEL : Q_CULEL;
		case Q_SINGLE:
			return Q_CLES;
		case Q_DOUBLE:
			return Q_CLED;
		default:
			assert(0);
		}
		break;
	case BIN_LAND:
	case BIN_LOR:
		assert(0); // Handled elsewhere to address short circuiting
	}
	assert(0); // Unreachable
}
