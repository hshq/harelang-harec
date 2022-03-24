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

fn reject() void = {
	assert(rt::compile("
		type s = null;
		fn test() void = {
			void;
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
};

export fn main() void = {
	basics();
	_nullable();
	reject();
};
