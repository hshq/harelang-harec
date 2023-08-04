use rt::{compile, exited, EXIT_SUCCESS};

type abool = bool;

fn lnot() void = {
	assert(!(false: abool));
};

fn addr() void = {
	let x = 42;
	let xptr = &x;
	assert(*xptr == 42);
	let y = &69;
	assert(*y == 69);
	let z = &struct { a: int = 42 };
	assert(z.a == 42);
	let a = -2;
	assert(a == 0 - 2);
	let b = 1-1;
	assert(b == 0);

	assert(compile("
		export fn main() void = { &null; };
	") as exited != EXIT_SUCCESS);
	assert(compile("
		export fn main() void = { &void; };
	") as exited != EXIT_SUCCESS);
	assert(compile("
		type foo = void;
		export fn main() void = { &foo; };
	") as exited != EXIT_SUCCESS);
	assert(compile("
		fn f() void = void;
		export fn main() void = { &f(); };
	") as exited != EXIT_SUCCESS);
};

export fn main() void = {
	lnot();
	addr();
};
