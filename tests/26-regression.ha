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

	assert(rt::compile("
		fn a() void = switch (b) {
		case &c => void;
		};"
	) as rt::exited != rt::EXIT_SUCCESS);

	assert(rt::compile("let a;") as rt::exited != rt::EXIT_SUCCESS);
};
