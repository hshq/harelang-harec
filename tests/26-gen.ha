// Tests for various gen bugs

type embedded = struct {
	a: u64,
	b: u8,
};

type thing = struct {
	offs: u64,
	e: embedded,
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

	let x: (void | int) = 10;
	match (x) {
	case i: int =>
		assert(i == 10);
	case void =>
		abort();
	};

	let p = 0;
	let p = &p: uintptr: u64: (u64 | void);
	let p = match (p) {
	case void =>
		abort();
	case p: u64 =>
		yield p: uintptr: *int;
	};
	assert(*p == 0);

        let thing: int = 0;
        let thing = &thing: (*int | int);
        let p = match (thing) {
	case int =>
		abort();
	case p: *int =>
		yield p;
        };
        *p = 0;
};
