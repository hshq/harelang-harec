fn c_abort(m: str) void;
export @symbol("rt.abort") fn abort_(m: str) void = c_abort(m);

fn c_abort_fixed(loc: str, i: int) void;
export @symbol("rt.abort_fixed") fn abort_fixed(loc: str, i: int) void =
	c_abort_fixed(loc, i);

fn c_memcpy(x: *void, y: *void, z: size) *void;
export @symbol("rt.memcpy") fn memcpy(x: *void, y: *void, z: size) *void = {
	return c_memcpy(x, y, z);
};

type string = struct {
	data: *[*]u8,
	length: size,
	capacity: size,
};

export @symbol("rt.strcmp") fn strcmp(_a: str, _b: str) bool = {
	if (len(_a) != len(_b)) {
		return false;
	};
	let a = (&_a: *string).data, b = (&_b: *string).data;
	for (let i = 0z; i < len(_a); i += 1) {
		if (a[i] != b[i]) {
			return false;
		};
	};
	return true;
};

export @symbol("malloc") fn c_malloc(n: size) nullable *void;
export @symbol("free") fn c_free(p: nullable *void) nullable *void;
export @symbol("realloc") fn c_realloc(p: nullable *void, n: size) nullable *void;

export @symbol("rt.malloc") fn malloc(n: size) nullable *void = {
	return c_malloc(n);
};

export @symbol("rt.free") fn free_(p: nullable *void) void = {
	c_free(p);
};

export @symbol("rt.realloc") fn realloc(p: nullable *void, n: size) nullable *void = {
	c_realloc(p, n);
};
