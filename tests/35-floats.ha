fn casts() void = {
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
};

export fn main() void = {
	// TODO: test parsing, compile-time evaluation
	casts();
};
