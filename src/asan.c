#include <assert.h>
#include "asan.h"
#include "gen.h"
#include "qbe.h"
#include "types.h"
#include "util.h"

void
gen_asan_redzone(struct gen_context *ctx,
		struct qbe_value *out, const struct type *type) {
	enum qbe_instr alloc = alloc_for_align(type->align);
	if (!(ctx->flags & GEN_SANITIZE_ADDRESS) || type->size != 8) {
		struct qbe_value sz = constl(type->size);
		pushprei(ctx->current, out, alloc, &sz, NULL);
		return;
	}

	// Force to 16-byte alignment
	// XXX: A cleverer approach could be used here
	alloc = Q_ALLOC16;

	const size_t extra = type->align * 4;
	struct qbe_value al2 = constl(type->align * 2);
	struct qbe_value rzsz = constl(type->size + extra);
	struct qbe_value redzone = mkqtmp(ctx, ctx->arch.ptr, "rz.%d");
	pushprei(ctx->current, &redzone, alloc, &rzsz, NULL);

	struct qbe_value shift = constl(3);
	struct qbe_value shadow_base = constl(0x7fff8000);
	struct qbe_value soff = mkqtmp(ctx, ctx->arch.ptr, "rz.o.%d");
	struct qbe_value sbase = mkqtmp(ctx, ctx->arch.ptr, "rz.p.%d");
	struct qbe_value sptr = mkqtmp(ctx, ctx->arch.ptr, "rz.p.%d");
	pushi(ctx->current, &soff, Q_SHR, &redzone, &shift, NULL);
	pushi(ctx->current, &sbase, Q_ADD, &soff, &shadow_base, NULL);

	// Poison redzone
	const struct qbe_value mask = constw(~0);
	const struct qbe_value step = constl(2);
	const struct qbe_value typeover8 = constl(type->size / 8);
	pushi(ctx->current, NULL, Q_STOREH, &mask, &sbase, NULL);
	pushi(ctx->current, &sptr, Q_ADD, &sbase, &step, NULL);
	pushi(ctx->current, &sptr, Q_ADD, &sptr, &typeover8, NULL);
	pushi(ctx->current, NULL, Q_STOREH, &mask, &sptr, NULL);

	struct gen_defer *defer = xcalloc(1, sizeof(struct gen_defer));
	defer->type = DEFER_REDZONE;
	defer->redzone.base = sbase;
	defer->redzone.typesz = type->size;
	defer->next = ctx->scope->defers;
	ctx->scope->defers = defer;

	pushi(ctx->current, out, Q_ADD, &redzone, &al2, NULL);
}

void gen_asan_redzone_cleanup(struct gen_context *ctx,
		struct gen_redzone *redzone) {
	struct qbe_value sbase = redzone->base;
	const struct qbe_value zero = constw(0);
	const struct qbe_value step = constl(2);
	const struct qbe_value typeover8 = constl(redzone->typesz / 8);
	pushi(ctx->current, NULL, Q_STOREH, &zero, &sbase, NULL);
	pushi(ctx->current, &sbase, Q_ADD, &sbase, &step, NULL);
	pushi(ctx->current, &sbase, Q_ADD, &sbase, &typeover8, NULL);
	pushi(ctx->current, NULL, Q_STOREH, &zero, &sbase, NULL);
}

void
gen_asan_loadcheck(struct gen_context *ctx,
		enum qbe_instr instr, struct qbe_value *addr) {
	if ((ctx->flags & GEN_SANITIZE_ADDRESS) == 0) {
		return;
	}
	if (instr != Q_LOADL) {
		return; // TODO
	}

	struct qbe_value soff = mkqtmp(ctx, ctx->arch.ptr, "s.off.%.d");
	struct qbe_value sptr = mkqtmp(ctx, ctx->arch.ptr, "s.ptr.%.d");
	struct qbe_value sval = mkqtmp(ctx, ctx->arch.ptr, "s.val.%.d");
	struct qbe_value shift = constl(3);
	struct qbe_value shadow_base = constl(0x7fff8000);
	pushi(ctx->current, &ctx->san_addr, Q_COPY, addr, NULL);
	pushi(ctx->current, &soff, Q_SHR, addr, &shift, NULL);
	pushi(ctx->current, &sptr, Q_ADD, &soff, &shadow_base, NULL);
	pushi(ctx->current, &sval, Q_LOADUB, &sptr, NULL);

	struct qbe_statement lvalid;
	struct qbe_value bvalid = mklabel(ctx, &lvalid, "s.valid.%.d");
	pushi(ctx->current, NULL, Q_JNZ, &sval, &ctx->san_error_load8, &bvalid, NULL);
	push(&ctx->current->body, &lvalid);
}
