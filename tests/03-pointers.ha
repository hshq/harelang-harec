use rt::{compile, exited, EXIT_SUCCESS};

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

	assert(compile(
		"fn test() void = { let x: nullable *int = null; let z = *x; };",
	) as exited != EXIT_SUCCESS);
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
	assert(a is *int);
};

fn reject() void = {
	assert(compile("
		type s = null;
		fn test() void = {
			void;
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		type s = *null;
		fn test() void = {
			void;
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a = &null;
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a = &3: null;
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let b: nullable *int = null;
			let a = b as null;
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a = (null, 3);
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a: []null = [null];
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a = [null];
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a: [_]null = [null];
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a = null;
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a: nullable *int = &4;
			a as int;
		};
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn test() void = {
			let a: nullable *int = &4;
			a as *str;
		};
	") as exited != EXIT_SUCCESS);

	// type assertions on non-nullable pointers are prohibited
	assert(compile("
		fn test() void = {
			let a: *int = &4;
			assert(a as *int);
		};
	") as exited != EXIT_SUCCESS);

	// dereference expression not in translation-compatible subset
	assert(compile("
		let a: int = 0;
		let b: *int = &a;
		let c: int = *b;
	") as exited != EXIT_SUCCESS);

	// dereference of a zero-sized type
	assert(compile("
		fn test() void = {
			let a: *void = &12;
			*a;
		};
	") as exited != EXIT_SUCCESS);
};

export fn main() void = {
	basics();
	_nullable();
	casts();
	reject();
};
