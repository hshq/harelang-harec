type abool = bool;

fn lnot() void = {
	assert((!*(&2: *bool))==false);
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
};

export fn main() void = {
	lnot();
	addr();
};
