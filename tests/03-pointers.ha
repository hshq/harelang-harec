use rt;

type intp = *int;

fn basics() void = {
	let x = 42;
	let y: intp = &x;
	assert(*y == 42);
	*y = 1337;
	assert(x == 1337);
};

fn _nullable() void = {
	let x: nullable *int = null;
	assert(x == null);
	let y = 42;
	x = &y;
	assert(*(x: *int) == 42);

	assert(rt::compile(
		"fn test() void = { let x: nullable *int = null; let z = *x; };",
	) != 0);
};

export fn main() void = {
	basics();
	_nullable();
};
