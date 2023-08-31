// Miscellaneous regression tests
use rt;

type embedded = struct {
	a: u64,
	b: u8,
};

type thing = struct {
	offs: u64,
	e: embedded,
};

def THING: thing = thing{
	offs = 0,
	e = embedded {
		a = 1,
		b = 0,
	},
};

let global: int = 0;

type mod::t = (size | const size); // needs to have a type id larger than size

// order of these three matters
type b = struct { c: c };
type a = struct { b };
type c = *a;

fn control_never() void = {
	let x = {
		yield yield 0;
	};
	assert(x == 0);
	x = {
		return yield 1;
	};
	assert(x == 1);
	x = {
		let x: int = yield 2;
		abort();
	};
	assert(x == 2);
	{
		for (true; yield) x += 1;
	};
	assert(x == 3);
	{
		for (let i: int = yield; true) x += 1;
	};
	assert(x == 3);
	{
		x = yield;
		x = 4;
	};
	assert(x == 3);
	{
		x += yield;
		abort();
	};
	assert({
		let x = true;
		x &&= yield x;
		abort();
	});

	if (false) {
		let x: int = abort();
		x += abort();
		let x = true;
		x &&= abort();
	};

	control_never1() as int;
	control_never2() as int;
};

fn control_never1() (int | void) = {
	return return 0;
};

fn control_never2() (int | void) = {
	{
		yield return 0;
	};
};

export fn main() void = {
	let t = thing {
		offs = 0,
		e = embedded {
			a = 1,
			b = 0,
		},
	};
	let t = t;
	assert(t.e.a == 1);

	let t2 = THING;
	assert(t2.e.a == 1);

	t2.offs = 42;
	assert(THING.offs == 0);

	let x: (void | int) = 10;
	match (x) {
	case let i: int =>
		assert(i == 10);
	case void =>
		abort();
	};

	let p = 0;
	let p = &p: uintptr: u64: (u64 | void);
	let p = match (p) {
	case void =>
		abort();
	case let p: u64 =>
		yield p: uintptr: *int;
	};
	assert(*p == 0);

	let thing: int = 0;
	let thing = &thing: (*int | int);
	let p = match (thing) {
	case int =>
		abort();
	case let p: *int =>
		yield p;
	};
	*p = 0;

	switch (&global) {
	case &global => void;
	case => abort();
	};

	match (void: (void | !void)) {
	case void => void;
	case !void => abort();
	};

	let s: []f64 = [1.0, 2.0, 3.0];
	s[..] = [0.0...];

	assert(rt::compile("
		fn a() void = switch (b) {
		case &c => void;
		};"
	) as rt::exited != rt::EXIT_SUCCESS);

	assert(rt::compile("let a;") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		type a = struct {
			b: int,
			c: int,
		};
		def A: a = a { b = 0 };"
	) as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("let a = &0;") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("def A: a = 1 % 1;") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("def A: b = void;") as rt::exited != rt::EXIT_SUCCESS);
	static assert(true == true && true != false);
	assert(rt::compile("
		type a = str;
		type b = struct { a };
		def A = b { c = 0 };"
	) as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		fn a() void = {
			let x: (void | int) = abort();
		};"
	) as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def A = 0;
		fn a() void = A = 0;"
	) as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("def A = x && true;") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("type a = struct { b: fn() void };") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("fn a() []int = alloc([]: [*]int, 0);") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("fn a() [1]int = [1]: []int: [1]int;") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("fn a() void = &*&a;") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("let a = [*&0];") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("fn a() *opaque = alloc(void);") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("fn a() void = { static let b = x & struct { a: int = 0 }; };") as rt::exited != rt::EXIT_SUCCESS);
	let a: (size | mod::t) = 0;
	assert(0xffu8 + 1 >> 1 == 0);
	assert(rt::compile("type a = *...b; type b = *...a;") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("def A = len([1]: []str);") as rt::exited != rt::EXIT_SUCCESS);

	control_never();
	assert(rt::compile("fn a() void = { abort(): int; };") as rt::exited != rt::EXIT_SUCCESS);
};
