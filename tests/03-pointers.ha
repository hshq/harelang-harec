use rt;

type intp = *int;

fn basics() void = {
	let x = 42;
	let y: intp = &x;
	assert(*y == 42);
	*y = 1337;
	assert(x == 1337);
};

fn _nullable() void = {
	let x: nullable *int = null;
	assert(x == null);
	let y = 42;
	x = &y;
	assert(*(x: *int) == 42);

	assert(rt::compile(
		"fn test() void = { let x: nullable *int = null; let z = *x; };",
	) != 0);
};

fn casts() void = {
	let a: *uint = &4u;
	let b = a: *void;
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
	assert((a as null): nullable *void == null);

	let a: nullable *int = &4;
	a as nullable *int;
};

fn reject() void = {
	assert(rt::compile("
		type s = null;
		fn test() void = {
			void;
		};
	") != 0);
	assert(rt::compile("
		type s = *null;
		fn test() void = {
			void;
		};
	") != 0);
	assert(rt::compile("
		fn test() void = {
			let a = &null;
		};
	") != 0);
	assert(rt::compile("
		fn test() void = {
			let a = &3: null;
		};
	") != 0);
	assert(rt::compile("
		fn test() void = {
			let b: nullable *int = null;
			let a = b as null;
		};
	") != 0);
	assert(rt::compile("
		fn test() void = {
			let a = (null, 3);
		};
	") != 0);
	assert(rt::compile("
		fn test() void = {
			let a: []null = [null];
		};
	") != 0);
	assert(rt::compile("
		fn test() void = {
			let a = [null];
		};
	") != 0);
	assert(rt::compile("
		fn test() void = {
			let a: [_]null = [null];
		};
	") != 0);
	assert(rt::compile("
		fn test() void = {
			let a = null;
		};
	") != 0);

	// type assertions on non-nullable pointers are prohibited
	assert(rt::compile("
		fn test() void = {
			let a: *int = &4;
			assert(a as *int);
		};
	") != 0);
};

export fn main() void = {
	basics();
	_nullable();
	casts();
	reject();
};
