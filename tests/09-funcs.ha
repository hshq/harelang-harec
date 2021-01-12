@noreturn fn rt::exit(status: int) void;

fn addone(x: *int) void = {
	*x += 1;
};

fn pointers() void = {
	let x = 0;
	addone(&x);
	assert(x == 1);
	let y = &addone;
	y(&x);
	assert(x == 2);
	let z = &y;
	z(&x);
	assert(x == 3);
};

fn vaargs(expected: []int, values: int...) void = {
	assert(len(expected) == len(values));
	for (let i = 0z; i < len(values); i += 1z) {
		assert(expected[i] == values[i]);
	};
};

let x: int = 42;

@init fn init() void = {
	x = 1337;
};

@fini fn fini() void = {
	rt::exit(42); // Magic number
};

export fn main() void = {
	pointers();
	vaargs([1, 2, 3], 1, 2, 3);
	assert(x == 1337);
};
