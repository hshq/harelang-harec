fn storage() void = {
	let x: (int, size) = (42, 1337);
	assert(size((int, size)) == size(size) * 2);
	let ptr = &x: *struct { i: int, z: size };
	assert(ptr.i == 42 && ptr.z == 1337);
};

fn indexing() void = {
	let x: (int, size) = (42, 1337);
	assert(x.0 == 42 && x.1 == 1337);
};

fn func(in: (int, size)) (int, size) = (in.0 + 1, in.1 + 1);

fn eval_expr_access() void = {
	static assert ((42, 0).0 == 42 && (42, 0).1 == 0);
};

fn eval_expr_tuple() void = {
	static let t = (42, 8);
};

fn funcs() void = {
	let x = func((41, 1336));
	assert(x.0 == 42 && x.1 == 1337);
};

// Regression tests for miscellaneous compiler bugs
fn regression() void = {
	let a: (((int | void), int) | void) = (void, 0);
};

export fn main() void = {
	storage();
	indexing();
	funcs();
	eval_expr_tuple();
	eval_expr_access();
	regression();
};
