use rt::{compile, status};

type inta = int;
type intp = *int;
type u32a = u32;
type func = fn() void;

fn basics() void = {
	let x = 42;
	let y: intp = &x;
	assert(*y == 42);
	let y: *int = y;
	assert(*y == 42);
	*y = 1337;
	assert(x == 1337);
	let z: *inta = &42;
	assert(*z == 42);
	let w: *const u32a = &42u32;
	assert(*w == 42);
	let a: *func = &basics;
	let b: *opaque = &42;

	assert(size(*int) == size(uintptr));
	assert(align(*int) == align(uintptr));
};

fn _nullable() void = {
	let x: nullable *int = null;
	assert(x == null);
	let y = 42;
	x = &y;
	assert(*(x: *int) == 42);

	compile(status::CHECK,
		"fn test() void = { let x: nullable *int = null; let z = *x; };")!;
};

fn casts() void = {
	let a: *uint = &4u;
	let b = a: *opaque;
	let c = b: *uint;
	assert(a == c && *c == 4);

	let a: nullable *uint = &7u;
	let b = a: *uint;
	assert(b == a && *b == 7);

	let a: nullable *uint = &10u;
	let b = a as *uint;
	assert(b == a && *b == 10);

	let a: nullable *int = &4;
	assert(a is *int);

	let a: nullable *int = null;
	assert(a is null);
	assert((a as null): nullable *opaque == null);

	let a: nullable *int = &4;
	assert(a is *int);

	let a = &42;
	let b = a: intp;
	assert(b == a && *b == 42);
};

fn reject() void = {
	compile(status::PARSE, "
		type s = null;
		fn test() void = {
			void;
		};
	")!;
	compile(status::PARSE, "
		type s = *null;
		fn test() void = {
			void;
		};
	")!;
	compile(status::CHECK, "
		fn test() void = {
			let a = &null;
		};
	")!;
	compile(status::PARSE, "
		fn test() void = {
			let a = &3: null;
		};
	")!;
	compile(status::PARSE, "
		fn test() void = {
			let a: nullable *int = &3: null;
		};
	")!;
	compile(status::CHECK, "
		fn test() void = {
			let b: nullable *int = null;
			let a = b as null;
		};
	")!;
	compile(status::CHECK, "
		fn test() void = {
			let a = (null, 3);
		};
	")!;
	compile(status::PARSE, "
		fn test() void = {
			let a: []null = [null];
		};
	")!;
	compile(status::CHECK, "
		fn test() void = {
			let a = [null];
		};
	")!;
	compile(status::PARSE, "
		fn test() void = {
			let a: [_]null = [null];
		};
	")!;
	compile(status::CHECK, "
		fn test() void = {
			let a = null;
		};
	")!;
	compile(status::CHECK, "
		fn test() void = {
			let a: nullable *int = &4;
			a as int;
		};
	")!;
	compile(status::CHECK, "
		fn test() void = {
			let a: nullable *int = &4;
			a as *str;
		};
	")!;

	// type assertions on non-nullable pointers are prohibited
	compile(status::CHECK, "
		fn test() void = {
			let a: *int = &4;
			assert(a as *int);
		};
	")!;

	// dereference expression not in translation-compatible subset
	compile(status::CHECK, "
		let a: int = 0;
		let b: *int = &a;
		let c: int = *b;
	")!;

	// can't cast to alias of opaque
	compile(status::CHECK, "
		type t = opaque;
		fn test() void = {
			let x: *t = &0;
		};
	")!;

	// can't have pointer to void
	compile(status::CHECK, "
		fn test() void = {
			let a: *void = &12;
		};
	")!;

	compile(status::CHECK, "
		fn test() void = {
			let a = &void;
		};
	")!;

	compile(status::CHECK, "
		fn test() void = {
			&12: *void;
		};
	")!;

	compile(status::CHECK, "
		fn f() void = {
			let a: nullable *void = null;
		};
	")!;

	// arithmetic on pointer types is not allowed
	compile(status::CHECK, `
		fn f() void = {
			let a = &12i + &8i;
		};
	`)!;

	compile(status::CHECK, `
		fn f() void = {
			let a = &12i + 8i: uintptr;
		};
	`)!;

	compile(status::CHECK, `
		fn f() void = {
			let a = &12i + 8i;
		};
	`)!;

	compile(status::CHECK, `
		fn f() void = {
			let a = &12i;
			a += &8i;
		};
	`)!;

	compile(status::CHECK, `
		fn f() void = {
			let a = &12i;
			a += 8i: uintptr;
		};
	`)!;

	compile(status::CHECK, `
		fn f() void = {
			let a = &12i;
			a += 8i;
		};
	`)!;

	// type promotion
	compile(status::CHECK, `
		fn test() void = {
			let a: *str = &0;
		};
	`)!;

	compile(status::CHECK, `
		fn test() void = {
			let a: *str = alloc(0);
		};
	`)!;

	// pointer to never
	compile(status::CHECK, `
		fn test() void = {
			&abort();
		};
	`)!;

	compile(status::CHECK, `
		fn test() nullable *never = null;
	`)!;

	compile(status::CHECK, `
		fn test() void = {
			*(&0: *never);
		};
	`)!;

	// pointer to zero-size type
	compile(status::CHECK, `
		fn test() void = {
			let a = &void;
		};
	`)!;

	compile(status::CHECK, `
		fn test() void = {
			let a = &(void, void);
		};
	`)!;

	compile(status::PARSE, `
		fn test() void = {
			let a = &(int, void).1;
		};
	`)!;

	compile(status::CHECK, `
		fn test() void = {
			let a = &([]: [0]int);
		};
	`)!;

	compile(status::CHECK, `
		fn test() void = {
			let a = &struct { v: void = void };
		};
	`)!;

	compile(status::CHECK, `
		fn test() void = {
			let a = &struct { i: int = 0, v: void = void }.v;
		};
	`)!;

	// eval
	compile(status::CHECK, `
		let a = 0;
		let b = 0;
		let c = &a == &b;
	`)!;

	compile(status::CHECK, `
		let a = 0;
		let b = 0;
		let c = &a < &b;
	`)!;

	compile(status::CHECK, `
		let a = 0;
		let b = &a: uintptr;
	`)!;
};

export fn main() void = {
	basics();
	_nullable();
	casts();
	reject();
};
