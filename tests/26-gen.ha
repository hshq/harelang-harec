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
		i: int => assert(i == 10),
		void => abort(),
	};

	let p = 0;
	let p = &p: uintptr: u64: (u64 | void);
	let p = match (p) {
		void => abort(),
		p: u64 => p: uintptr: *int,
	};
	assert(*p == 0);

        let thing: int = 0;
        let thing = &thing: (*int | int);
        let p = match (thing) {
                int => abort(),
                p: *int => p,
        };
        *p = 0;
};
