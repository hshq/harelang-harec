use rt;

fn padding() void = {
	assert(size(struct { x: i32, y: i32 }) == 8);
	assert(size(struct { x: i32, y: i64 }) == 16);
	assert(size(union { x: i8, y: i16, z: i32 }) == 4);

	const s = struct {
		x: i8 = 10,
		y: i16 = 20,
		z: i32 = 30,
		q: i64 = 40,
	};
	assert(&s.x: uintptr: size % 1 == 0);
	assert(&s.y: uintptr: size % 2 == 0);
	assert(&s.z: uintptr: size % 4 == 0);
	assert(&s.q: uintptr: size % 8 == 0);
};

fn storage() void = {
	let coords = struct { x: i32 = 10, y: i32 = 20 };
	let ptr = &coords: *[*]i32;
	assert(ptr[0] == 10 && ptr[1] == 20);
};

fn assignment() void = {
	let coords = struct { x: int = 20, y: int = 30 };
	coords.x = 40;
	coords.y = 50;
	assert(coords.x == 40 && coords.y == 50);
	coords = struct { x: int = 60, y: int = 70 };
	assert(coords.x == 60 && coords.y == 70);
};

fn deref() void = {
	let coords = struct { x: int = 20, y: int = 30 };
	let a = &coords;
	assert(a.x == 20 && a.y == 30);
	let b = &a;
	assert(b.x == 20 && b.y == 30);
	let c = &b;
	assert(c.x == 20 && c.y == 30);
	c.x = 42;
	c.y = 96;
	assert(coords.x == 42 && coords.y == 96);
};

type embedded = struct {
	foo: u8,
};

type embed1 = struct {
	embedded,
	struct {
		bar: u8,
		baz: u64,
	},
};

type embed2 = struct {
	struct {
		bar: u8,
		baz: u64,
	},
	embedded,
};

fn nested() void = {
	let s = embed1 {
		foo = 42,
		bar = 69,
		baz = 1337,
	};
	assert(s.foo == 42 && s.bar == 69 && s.baz == 1337);
	let s = embed2 {
		foo = 42,
		bar = 69,
		baz = 1337,
	};
	assert(s.foo == 42 && s.bar == 69 && s.baz == 1337);

	let s = struct {
		x: int = 10,
		y: struct {
			z: int,
			q: int,
			a: struct { b: int, c: int },
		} = struct {
			z: int = 20,
			q: int = 30,
			a: struct { b: int, c: int } = struct {
				b: int = 42, c: int = 24,
			},
		},
	};
	assert(s.x == 10);
	assert(s.y.z == 20);
	assert(s.y.q == 30);
	assert(s.y.a.b == 42);
	assert(s.y.a.c == 24);
	assert(&s.y: uintptr == &s.y.z: uintptr);

	s.x = 1337;
	assert(s.x == 1337);

	s.y.a = struct { b: int = 1337, c: int = 7331 };
	assert(s.y.a.b == 1337);
	assert(s.y.a.c == 7331);
};

type coords = struct { x: int, y: int };

fn named() void = {
	let x = coords { y = 10, x = 20 };
	assert(x.x == 20 && x.y == 10);
};

fn autofill() void = {
	let x = coords { x = 10, ... };
	assert(x.x == 10 && x.y == 0);
};

fn invariants() void = {
	const failures = [
	// Assign field from non-assignable type:
	"fn test() void = { let x: struct { y: int } = struct { y: int = 10u }; };",

	"
	type coords = struct { x: int, y: int };

	fn test() void = {
		let x = coords { x: int = 10u, y: int = 20u };
	};
	",

	// Duplicate members
	"fn test() void = { let x: struct { a: int, a: int } = struct { a: int = 2 };"
	// Dereference non-nullable pointer:
	"fn test() void = { let x: nullable *struct { y: int } = null; x.y; };",
	// Select field from non-struct object:
	"fn test() void = { let x = 10; x.y; };",
	// Unknown field:
	"fn test() void = { let x: struct { y: int } = struct { y: int = 10 }; x.z; };",
	// Untyped field for unnamed struct:
	"fn test() void = { let x = struct { x = 10 }; };",
	];
	for (let i = 0z; i < len(failures); i += 1) {
		assert(rt::compile(failures[i]) != 0);
	};
};

fn fields() void = {
	let n: u32 = 0;

	let up: *union {
		struct {
			a: u8,
			b: u8,
		},
		c: u32,
	} = &n: uintptr: *union {
		struct {
			a: u8,
			b: u8,
		},
		c: u32,
	};
	assert(&up.a: uintptr == &n: uintptr);
	assert(&up.b: uintptr == &n: uintptr + 1);
	assert(&up.c: uintptr == &n: uintptr);

	let sp: *struct {
		a: u8,
		struct {
			b: u8,
			struct {
				c: u8,
			},
		},
	} = &n: uintptr: *struct {
		a: u8,
		struct {
			b: u8,
			struct {
				c: u8,
			},
		},
	};
	assert(&sp.a: uintptr == &n: uintptr);
	assert(&sp.b: uintptr == &n: uintptr + 1);
	assert(&sp.c: uintptr == &n: uintptr + 2);
};

export fn main() void = {
	padding();
	storage();
	assignment();
	deref();
	nested();
	named();
	autofill();
	invariants();
	fields();
	// TODO:
	// - Union tests
	// - Embedded structs
	// - offset()
};
