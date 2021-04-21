fn error() bool = {
	abort();
	return false;
};

fn set(x: *int) bool = {
	*x = 42;
	return true;
};

fn andor() void = {
	assert((false || false) == false);
	assert((false || true) == true);
	assert((true || false) == true);
	assert((true || true) == true);
	assert((true || error()) == true);
	let x = 0;
	assert((false || set(&x)) == true);
	assert(x == 42);

	assert((false && false) == false);
	assert((false && true) == false);
	assert((true && false) == false);
	assert((true && true) == true);
	assert((false && error()) == false);
	x = 0;
	assert((true && set(&x)) == true);
	assert(x == 42);
};

fn sar_shr() void = {
	assert(-12697259629065987i64 >> 26 == -189203913);

	let x = 1i64;
	x <<= 63;
	assert(x == -9223372036854775808);
	x >>= 63;
	assert(x == -1);

	let y = 1u64;
	y <<= 63;
	assert(y == 9223372036854775808);
	y >>= 63;
	assert(y == 1);

	assert(-4i32 >> 1 == -2);

	let h0 = -12697259629065987i64;
	let h1 = (h0 + (1i64 << 25)) >> 26;
	assert(h1 == -189203912);
};

export fn main() void = {
	// TODO: other binarithms
	andor();
	sar_shr();
};
