use rt;
use rt::{compile, exit, status};

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
	rt::exit_status = 42;
};

@init fn init() void = {
	void; // Should be allowed to have the same name
};

@fini fn fini() void = {
	rt::exit_status = 0;
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
		let arg = vaarg(ap, int);
		assert(arg == i + 1);
	};
	for (let i = 0; i < n; i += 1) {
		let arg = vaarg(ap2, int);
		vaarg(ap2, void);
		vaarg(ap2, [0]int);
		assert(arg == i + 1);
	};
};

fn cvafn2(...) void = {
	let ap = vastart();
	defer vaend(ap);
	assert(vaarg(ap, int) == 1337);
	assert(vaarg(ap, int) == 42);
};

fn cvaargs() void = {
	cvafn(3, 1, 2, 3);
	cvafn2(1337, 42);
	cvafn(3, void, 1, 2, void, 3);
	cvafn2(void, 1337, void, void, 42);
};

fn reject() void = {
	compile(status::PARSE, "fn f(x: int..., ...) void = void;")!;

	let failures: [_]str = [
		// parameter of undefined size
		"fn f(x: [*]int) void = void;",
		"fn f(x: [*]int...) void = void;",
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

		// non-prototype declarations with unnamed parameters
		"fn f(int) void = void;",
		"fn f(*int) void = void;",
		"type t = void; fn f(t) void = void;",

		// variadic call to a function that does not support that
		"fn a(x: int) void = void; fn b() void = { a(5...); };",
		"fn a(x: int, ...) void = void; fn b() void = { a(5...); };",
		"fn a(x: []int) void = void; fn b() void = { a([5]...); };",
		"fn a(x: []int, ...) void = void; fn b() void = { a([5]...); };",

		// unresolved parameter type
		"fn f(arg: T) void; fn g() void = f(1);",
		"fn f(arg: T...) void; fn g() void = f(1);",
		"fn f(arg: T, ...) void; fn g() void = f(1);",

		// required params may not follow optional
		"fn f(a: int = 5, b: int) void = void;",
		// default value must be assignable to type
		"fn f(a: str = 5) void = void;",

		// too many arguments
		"fn f() void = f(0);",
		"fn f(x: int) void = f(0, 1);",
		// not enough arguments
		"fn f(x: int) void = f();",
		"fn f(x: int, y: int...) void = f();",
		"fn f(x: int, y: int) void = f(0);",
		"fn f(x: int, y: int, z: int...) void = f(0);",
		// argument type mismatch
		"fn f(x: str) void = f(0);",
		"fn f(x: str = \"asdf\") void = f(0);",
		"fn f(x: str...) void = f(0);",
		"fn f(x: str...) void = f(\"asdf\", 0);",
		// cannot return a type with undefined size
		"fn f() fn() void = g; fn g() void = void;",

		"fn f(ap: valist) void = { vaarg(ap, opaque); };",
	];

	for (let i = 0z; i < len(failures); i += 1) {
		compile(status::CHECK, failures[i])!;
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

type int_alias = int;

fn optional_simple(a: int, b: int = 3) int = b;
fn optional_str(x: str = "hi") str = x;
fn optional_add(a: int = 3, b: int = 2) int = a + b;
fn optional_tagged(x: (int | void) = void) (int | void) = x;
fn optional_tuple(x: (int, str) = (42, "hi")) str = x.1;
fn optional_alias(x: int_alias = 3) int_alias = x;
fn optional_alias2(x: int = 3: int_alias) int = x;
fn optional_slc(x: []int = []) size = len(x);
fn optional_slc2(x: []int = [1, 2]) int = {
	// Call repeatedly in a loop to check that the slice is fresh each time.
	assert(x[1] == 2);
	x[1] = 3;
	return x[0];
};
fn optional_ptr(x: nullable *int = null) bool = x is null;
fn optional_variadic(a: int = 3, b: int...) bool = a == 3 || len(b) == 2;
fn optional_variadic_c(n: int = 0, ...) void = {
	let ap = vastart();
	defer vaend(ap);
	for (let i = 0; i < n; i += 1) {
		assert(vaarg(ap, int) == i);
	};
};

fn optional_params() void = {
	assert(optional_simple(5, 1) == 1);
	assert(optional_simple(5) == 3);
	assert(optional_str() == "hi");
	assert(optional_str("hello") == "hello");
	assert(optional_add() == 5);
	assert(optional_add(1) == 3);
	assert(optional_add(1, 1) == 2);
	assert(optional_tagged() is void);
	assert(optional_tagged(5) as int == 5);
	assert(optional_tuple() == "hi");
	assert(optional_tuple((12, "wow")) == "wow");
	assert(optional_alias() == 3);
	assert(optional_alias(1: int_alias) == 1);
	assert(optional_alias2() == 3);
	assert(optional_alias2(1: int_alias) == 1);
	assert(optional_slc() == 0);
	assert(optional_slc([1, 2, 3]) == 3);
	for (let i = 0; i < 3; i += 1) {
		assert(optional_slc2() == 1);
		assert(optional_slc2([2, 2]) == 2);
	};
	assert(optional_ptr());
	let i = 0;
	assert(!optional_ptr(&i));
	assert(optional_variadic());
	assert(optional_variadic(3));
	assert(optional_variadic(0, 1, 2));
	optional_variadic_c();
	optional_variadic_c(0);
	optional_variadic_c(1, 0);
	optional_variadic_c(3, 0, 1, 2);
};

export fn main() void = {
	assert(simple() == 69);
	pointers();
	vaargs();
	cvaargs();
	zerosizeparams();
	assert(x == 1337);
	reject();
	optional_params();
	_never();
};
