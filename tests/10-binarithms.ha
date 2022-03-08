fn error() bool = {
	abort();
};

fn set(x: *int) bool = {
	*x = 42;
	return true;
};

fn andorxor() void = {
	assert((false || false) == false);
	assert((false || true) == true);
	assert((true || false) == true);
	assert((true || true) == true);
	assert((true || error()) == true);
	let x = 0;
	assert((false || set(&x)) == true);
	assert(x == 42);

	let x = 0;
	let f = false;
	f ||= false;
	assert(!f);
	f ||= set(&x);
	assert(x == 42);
	assert(f);
	f || error();
	assert(f);
	f ||= false;
	assert(f);

	assert((false && false) == false);
	assert((false && true) == false);
	assert((true && false) == false);
	assert((true && true) == true);
	assert((false && error()) == false);
	x = 0;
	assert((true && set(&x)) == true);
	assert(x == 42);

	let x = 0;
	let f = true;
	f &&= true;
	f &&= set(&x);
	assert(x == 42);
	assert(f);
	f &&= false;
	assert(!f);
	f &&= error();
	f &&= true;
	assert(!f);

	assert((false ^^ false) == false);
	assert((false ^^ true) == true);
	assert((true ^^ false) == true);
	assert((true ^^ true) == false);

	let f = true;
	f ^^= true;
	assert(!f);
	f ^^= false;
	assert(!f);
	f ^^= true;
	assert(f);
	f ^^= false;
	assert(f);
};

fn sar_shr() void = {
	assert(-12697259629065987i64 >> 26 == -189203913);

	let x = 1i64;
	x <<= 63;
	assert(x == -9223372036854775808i64);
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

def FLOAT: f64 = 6.0 * 7.0;
def I8: i8 = 127 * 2;
def U8: u8 = 128 * 2;

fn eval() void = {
	assert(FLOAT == 42.0);
	assert(I8 == 126);
	assert(U8 == 0);
};

export fn main() void = {
	// TODO: other binarithms
	andorxor();
	sar_shr();
	eval();
};
