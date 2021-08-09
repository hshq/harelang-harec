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
