fn measurements() void = {
	assert(size(f32) == align(f32));
	assert(size(f32) == align(f32));
};

fn signed_casts() void = {
	let a = 20f64;
	let f = a: f32;
	assert(f == 20f32);
	let i = a: i8;
	assert(i == 20);
	let i = a: i16;
	assert(i == 20);
	let i = a: i16;
	assert(i == 20);
	let i = a: i32;
	assert(i == 20);
	let i = a: i64;
	assert(i == 20);

	let a = 20f32;
	let f = a: f64;
	assert(f == 20f64);
	let i = a: i8;
	assert(i == 20);
	let i = a: i16;
	assert(i == 20);
	let i = a: i16;
	assert(i == 20);
	let i = a: i32;
	assert(i == 20);
	let i = a: i64;
	assert(i == 20);

	let a = 20i8;
	let f = a: f32;
	assert(f == 20f32);
	let f = a: f64;
	assert(f == 20f64);

	let a = 20i16;
	let f = a: f32;
	assert(f == 20f32);
	let f = a: f64;
	assert(f == 20f64);

	let a = 20i32;
	let f = a: f32;
	assert(f == 20f32);
	let f = a: f64;
	assert(f == 20f64);

	let a = 20i64;
	let f = a: f32;
	assert(f == 20f32);
	let f = a: f64;
	assert(f == 20f64);

	let f = 20: f32;
	assert(f == 20f32);
	let f = 20: f64;
	assert(f == 20f64);
	let i = 13.37: int;
	assert(i == 13);
};

fn unsigned_casts() void = {
	let a = 20f64;
	let f = a: f32;
	assert(f == 20f32);
	let i = a: u8;
	assert(i == 20);
	let i = a: u16;
	assert(i == 20);
	let i = a: u16;
	assert(i == 20);
	let i = a: u32;
	assert(i == 20);
	let i = a: u64;
	assert(i == 20);

	let a = 20f32;
	let f = a: f64;
	assert(f == 20f64);
	let i = a: u8;
	assert(i == 20);
	let i = a: u16;
	assert(i == 20);
	let i = a: u16;
	assert(i == 20);
	let i = a: u32;
	assert(i == 20);
	let i = a: u64;
	assert(i == 20);

	let a = 20u8;
	let f = a: f32;
	assert(f == 20f32);
	let f = a: f64;
	assert(f == 20f64);

	let a = 20u16;
	let f = a: f32;
	assert(f == 20f32);
	let f = a: f64;
	assert(f == 20f64);

	let a = 20u32;
	let f = a: f32;
	assert(f == 20f32);
	let f = a: f64;
	assert(f == 20f64);

	let a = 20u64;
	let f = a: f32;
	assert(f == 20f32);
	let f = a: f64;
	assert(f == 20f64);

	let i = 13.37: uint;
	assert(i == 13);
};

fn eval() void = {
	// TODO: more compile-time evaluation tests
	assert(0.3f32: f64 != 0.3);
	static assert(0.3f32: f64 != 0.3);
};

export fn main() void = {
	// TODO: test parsing
	measurements();
	signed_casts();
	unsigned_casts();
	eval();
};
