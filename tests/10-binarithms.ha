use rt::{compile, exited, EXIT_SUCCESS};
use testmod;

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

	static assert((false || false) == false);
	static assert((false || true) == true);
	static assert((true || false) == true);
	static assert((true || true) == true);

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

	static assert((false && false) == false);
	static assert((false && true) == false);
	static assert((true && false) == false);
	static assert((true && true) ==  true);

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

	static assert((false ^^ false) == false);
	static assert((false ^^ true) == true);
	static assert((true ^^ false) == true);
	static assert((true ^^ true) == false);

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

fn arithmetic() void = {
	assert(1337 + 1234 == 2571);
	assert(1337 - 1234 == 103);
	assert(1234 - 1337 == -103);
	assert(1337 * 1234 == 1649858);
	assert(1337 / 1234 == 1);
	assert(1234 / 1337 == 0);
	assert(625 / 5 == 125);
	assert(1337 % 1234 == 103);
	assert(1234 % 1337 == 1234);
	assert(625 % 5 == 0);
	assert(2147483647i32 + 1 == -2147483648i32);
	assert(-2147483648i32 - 1 == 2147483647i32);
	assert(4294967295u32 + 1 == 0u32);
	assert(0u32 - 1 == 4294967295u32);
	assert(-1337 * 1234 == -1649858);
	assert(1337 * -1234 == -1649858);
	assert(-1337 * -1234 == 1649858);
	assert(-1337 / 1234 == -1);
	assert(1337 / -1234 == -1);
	assert(-1337 / -1234 == 1);
	assert(-1234 / 1337 == 0);
	assert(1234 / -1337 == 0);
	assert(-1234 / -1337 == 0);
	assert(-625 / 5 == -125);
	assert(625 / -5 == -125);
	assert(-625 / -5 == 125);
	assert(-1337 % 1234 == -103);
	assert(1337 % -1234 == 103);
	assert(-1337 % -1234 == -103);
	assert(-1234 % 1337 == -1234);
	assert(1234 % -1337 == 1234);
	assert(-1234 % -1337 == -1234);
	assert(-625 % 5 == 0);
	assert(625 % -5 == 0);
	assert(-625 % -5 == 0);

	static assert(1337 + 1234 == 2571);
	static assert(1337 - 1234 == 103);
	static assert(1234 - 1337 == -103);
	static assert(1337 * 1234 == 1649858);
	static assert(1337 / 1234 == 1);
	static assert(1234 / 1337 == 0);
	static assert(625 / 5 == 125);
	static assert(1337 % 1234 == 103);
	static assert(1234 % 1337 == 1234);
	static assert(625 % 5 == 0);
	static assert(2147483647i32 + 1 == -2147483648i32);
	static assert(-2147483648i32 - 1 == 2147483647i32);
	static assert(4294967295u32 + 1 == 0u32);
	static assert(0u32 - 1 == 4294967295u32);
	static assert(-1337 * 1234 == -1649858);
	static assert(1337 * -1234 == -1649858);
	static assert(-1337 * -1234 == 1649858);
	static assert(-1337 / 1234 == -1);
	static assert(1337 / -1234 == -1);
	static assert(-1337 / -1234 == 1);
	static assert(-1234 / 1337 == 0);
	static assert(1234 / -1337 == 0);
	static assert(-1234 / -1337 == 0);
	static assert(-625 / 5 == -125);
	static assert(625 / -5 == -125);
	static assert(-625 / -5 == 125);
	static assert(-1337 % 1234 == -103);
	static assert(1337 % -1234 == 103);
	static assert(-1337 % -1234 == -103);
	static assert(-1234 % 1337 == -1234);
	static assert(1234 % -1337 == 1234);
	static assert(-1234 % -1337 == -1234);
	static assert(-625 % 5 == 0);
	static assert(625 % -5 == 0);
	static assert(-625 % -5 == 0);
};

def FLOAT: f64 = 6.0 * 7.0;
def I8: i8 = 127 * 2;
def U8: u8 = 128 * 2;
def ALIAS: testmod::enum_alias = 1: testmod::_enum: testmod::enum_alias + 1: testmod::enum_alias;
let a: i8 = 3i8 - (-128i8);
let b: i8 = 3i8 + (-128i8);
def A: i8 = 3i8 - (-128i8);
def B: i8 = 3i8 + (-128i8);
def I32: i32 = 3 - (-2147483648i32);

fn eval() void = {
	assert(FLOAT == 42.0);
	assert(I8 == -2i8);
	assert(U8 == 0);
	assert(a == -125i8);
	assert(b == -125i8);
	assert(A == -125i8);
	assert(B == -125i8);
	assert(I32 == -2147483645i32);
	assert(ALIAS == 2);

	static assert(FLOAT == 42.0);
	static assert(I8 == -2i8);
	static assert(U8 == 0);
	static assert(A == -125i8);
	static assert(B == -125i8);
	static assert(I32 == -2147483645i32);
	static assert(ALIAS == 2);
};

fn reject() void = {
	assert(compile("let x = 1 / 0;") as exited != EXIT_SUCCESS);
	assert(compile("let x = -2147483648i32 / -1;") as exited != EXIT_SUCCESS);
	assert(compile("let x = 1 % 0;") as exited != EXIT_SUCCESS);
	assert(compile("let x = -2147483648i32 % -1;") as exited != EXIT_SUCCESS);
};

export fn main() void = {
	// TODO: other binarithms
	andorxor();
	sar_shr();
	arithmetic();
	eval();
	reject();
};
