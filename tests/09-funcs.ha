use rt;
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
	rt::status = 42;
};

@init fn init() void = {
	void; // Should be allowed to have the same name
};

@fini fn fini() void = {
	rt::status = 0;
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

fn cvafn2(...) void = {
	let ap = vastart();
	defer vaend(ap);
	assert(vaarg(ap): int == 1337);
	assert(vaarg(ap): int == 42);
};

fn cvaargs() void = {
	cvafn(3, 1, 2, 3);
	cvafn2(1337, 42);
	cvafn(3, void, 1, 2, void, 3);
	cvafn2(void, 1337, void, void, 42);
};

fn reject() void = {
	let failures: [_]str = [
		// parameter of undefined size
		"fn f(x: [*]int) void = void;",
		// return value of undefined size
		"fn f(x: int) [*]int = void;",

		"let x: size = size(fn(x: int) int);",
		"let x: size = align(fn(x: int) int);",

		// @test functions are always typechecked
		"@test fn test() void = 42 + \"foo\";"
		"let x: int = 4;", // ensure there is at least one declaration

		"fn f(x: int) void = void;"
		"fn g(x: int) void = void;"
		"fn test() void = { f + g; };",
		"fn test() void = { test = test; };",

		"fn f() void = { size(*never); };",
	];

	for (let i = 0z; i < len(failures); i += 1) {
		assert(compile(failures[i]) as exited != EXIT_SUCCESS);
	};
};

type zerostruct = struct { v: void };
type zerounion = union { v: void };

fn zeroparamfunc(
	a: int,
	b: void,
	c: str,
	d: [0]int,
	e: f64,
	f: zerostruct,
	g: zerounion,
	h: *int,
	i: (void, void),
	j: int,
) void = {
	assert(a == 1);
	b;
	assert(c == "2");
	d;
	assert(len(d) == 0);
	assert(e == 3.0);
	f; f.v;
	g; g.v;
	assert(*h == 4);
	i; i.0; i.1;
	assert(j == 5);
};

fn zerosizeparams() void = {
	zeroparamfunc(1, void, "2", [], 3.0, zerostruct { ... },
		zerounion { ... }, &4, (void, void), 5);
};

fn neverfunc(x: int) never = if (x == 1) exit(0) else abort();

fn _never() void = {
	if (false) neverfunc(0);
	if (true) neverfunc(neverfunc(1));
	abort();
};

export fn main() void = {
	assert(simple() == 69);
	pointers();
	vaargs();
	cvaargs();
	zerosizeparams();
	assert(x == 1337);
	reject();
	_never();
};
