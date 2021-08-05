fn integers() void = {
	let x: u8 = 42;
	let y = x: int;
	assert(y == 42);

	let x = 0xBEEF;
	let y = x: u8;
	assert(y == 0xEF);

	let x = -10i8;
	let y = x: u8;
	assert(y == 246);
};

fn floats() void = {
	let x = 10;
	let y = x: f32;
	assert(y == 10f32);

	let x = 10;
	let y = x: f64;
	assert(y == 10f64);

	let x = 13.37f32;
	let y = x: int;
	assert(y == 13);

	let x = 13.37f64;
	let y = x: int;
	assert(y == 13);
};

export fn main() int = {
	integers();
	floats();
	return 0;
};
