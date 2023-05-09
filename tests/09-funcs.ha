use rt::{compile, exit, exited, EXIT_SUCCESS};

fn simple() int = return 69;

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

fn vafn(expected: []int, values: int...) void = {
	assert(len(expected) == len(values));
	for (let i = 0z; i < len(values); i += 1) {
		assert(expected[i] == values[i]);
	};
};

fn vaargs() void = {
	vafn([1, 2, 3], 1, 2, 3);
	let data = [1, 2, 3];
	vafn(data, data...);
	vafn([]);
};

let x: int = 42;

@init fn init() void = {
	x = 1337;
};

@init fn init() void = {
	void; // Should be allowed to have the same name
};

@fini fn fini() void = {
	exit(42); // Magic number
};

@fini fn fini() void = {
	void; // Should be allowed to have the same name
};

fn cvafn(n: int, ...) void = {
	let ap = vastart();
	defer vaend(ap);
	let ap2 = ap;
	defer vaend(ap2);
	for (let i = 0; i < n; i += 1) {
		let arg: int = vaarg(ap);
		assert(arg == i + 1);
	};
	for (let i = 0; i < n; i += 1) {
		let arg: int = vaarg(ap2);
		assert(arg == i + 1);
	};
};

fn cvaargs() void = {
	cvafn(3, 1, 2, 3);
};

fn reject() void = {
	let failures: [_]str = [
		// parameter of size 0
		"fn f(x: void) void = void;",
		// parameter of undefined size
		"fn f(x: [*]int) void = void;",
		// return value of undefined size
		"fn f(x: int) [*]int = void;",

		"let x: size = size(fn(x: int) int);",
		"let x: size = align(fn(x: int) int);",

		// @test functions are always typechecked
		"@test fn test() void = 42 + \"foo\";"
		"let x: int = 4;", // ensure there is at least one declaration
	];

	for (let i = 0z; i < len(failures); i += 1) {
		assert(compile(failures[i]) as exited != EXIT_SUCCESS);
	};
};

export fn main() void = {
	assert(simple() == 69);
	pointers();
	vaargs();
	cvaargs();
	assert(x == 1337);
	reject();
};
