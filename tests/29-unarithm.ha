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
	let q = &{ yield 42; };
	assert(*q == 42);
	*{ yield q; } = 12;
	assert(*q == 12);

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

fn neg() void = {
	let a = -2;
	assert(a == 0 - 2);
	let b = 1-1;
	assert(b == 0);
};

fn deref() void = {
	assert(*{ yield &42; } == 42);
	assert(*switch (0) { case => yield &42; } == 42);
	assert(*match (0: (int | void)) { case => yield &42; } == 42);

	assert(compile("
		export fn main() void = { *static assert(true); };
	") as exited != EXIT_SUCCESS);
	assert(compile("
		export fn main() void = { *if (true) &42 else &1337; };
	") as exited != EXIT_SUCCESS);
};

export fn main() void = {
	lnot();
	addr();
	neg();
	deref();
};
