def PAGE_SIZE: uint = 4096u;

export fn malloc(n: size) nullable *void = {
	let p: *void = mmap(null, n,
		PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANON, -1, 0);
	return if (p: uintptr: int == -ENOMEM) null else p: nullable *void;
};

export fn must_malloc(n: size) *void = {
	let p = malloc(n);
	assert(p != null, "out of memory");
	return p: *void;
};

export @symbol("sys.free") fn _free() void = {
	// no-op
	return;
};

export fn realloc() nullable *void = {
	assert(false, "realloc: not supported");
	return null;
};

export fn must_realloc() *void = {
	assert(false, "must_realloc: not supported");
	return null;
};
