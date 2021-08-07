fn c_abort(m: str) void;
export @symbol("rt.abort") fn abort_(m: str) void = c_abort(m);

fn c_abort_fixed(loc: str, i: int) void;
export @symbol("rt.abort_fixed") fn abort_fixed(loc: str, i: int) void =
	c_abort_fixed(loc, i);

fn c_memcpy(x: *void, y: *void, z: size) *void;
export @symbol("rt.memcpy") fn memcpy(x: *void, y: *void, z: size) *void = {
	return c_memcpy(x, y, z);
};
