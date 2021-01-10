fn rt::compile(src: str) int;

fn padding() void = {
	assert(size(struct { x: i32, y: i32 }) == 8z);
	assert(size(struct { x: i32, y: i64 }) == 16z);
	assert(size(union { x: i8, y: i16, z: i32 }) == 4z);

	const s = struct {
		x: i8 = 10i8,
		y: i16 = 20i16,
		z: i32 = 30i32,
		q: i64 = 40i32,
	};
	assert(&s.x: uintptr: size % 1z == 0z);
	assert(&s.y: uintptr: size % 2z == 0z);
	assert(&s.z: uintptr: size % 4z == 0z);
	assert(&s.q: uintptr: size % 8z == 0z);
};

fn storage() void = {
	let coords = struct { x: i32 = 10i32, y: i32 = 20i32 };
	let ptr = &coords: *[*]i32;
	assert(ptr[0] == 10i32);
	assert(ptr[1] == 20i32);
};

fn assignment() void = {
	let coords = struct { x: int = 20, y: int = 30 };
	coords.x = 40;
	coords.y = 50;
	assert(coords.x == 40);
	assert(coords.y == 50);
	coords = struct { x: int = 60, y: int = 70 };
	assert(coords.x == 60);
	assert(coords.y == 70);
};

fn deref() void = {
	let coords = struct { x: int = 20, y: int = 30 };
	let a = &coords;
	assert(a.x == 20);
	assert(a.y == 30);
	let b = &a;
	assert(b.x == 20);
	assert(b.y == 30);
	let c = &b;
	assert(c.x == 20);
	assert(c.y == 30);
	c.x = 42;
	c.y = 96;
	assert(coords.x == 42);
	assert(coords.y == 96);
};

fn nested() void = {
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

fn invariants() void = {
	const failures = [
	// Assign field from non-assignable type:
	"fn test() void = { let x: struct { y: int } = struct { y: int = 10u }; };",
	// Dereference non-nullable pointer:
	"fn test() void = { let x: nullable *struct { y: int } = null; x.y; };",
	// Select field from non-struct object:
	"fn test() void = { let x = 10; x.y; };",
	// Unknown field:
	"fn test() void = { let x: struct { y: int } = struct { y: int = 10 }; x.z; };",
	];
	for (let i = 0z; i < len(failures); i += 1z) {
		assert(rt::compile(failures[i]) != 0);
	};
};

export fn main() void = {
	padding();
	storage();
	assignment();
	deref();
	nested();
	invariants();
	// TODO:
	// - Union tests
	// - Named structs
	// - Embedded structs
	// - offset()
	// - Autofilling (...) and its invariants
};
