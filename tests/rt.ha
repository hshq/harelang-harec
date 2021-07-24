@symbol("abort") fn c_abort() void;
export @symbol("rt.abort") fn abort_() void = c_abort();

fn c_memcpy(x: *void, y: *void, z: size) *void;
export @symbol("rt.memcpy") fn memcpy(x: *void, y: *void, z: size) *void = {
	return c_memcpy(x, y, z);
};
