fn basics() void = {
	let x = 42;
	let y: *int = &x;
	assert(*y == 42);
	*y = 1337;
	assert(x == 1337);
};

fn _nullable() void = {
	let x: nullable *int = null;
	assert(x == null: nullable *int); // TODO: type promotion
	let y = 42;
	x = &y;
	assert(*(x: *int) == 42);
	// TODO: Expected failures with nullable pointers
};

export fn main() void = {
	basics();
	_nullable();
};
