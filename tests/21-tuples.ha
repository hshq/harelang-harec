use rt;

fn storage() void = {
	let x: (int, size) = (42, 1337);
	assert(size((int, size)) == size(size) * 2);
	let ptr = &x: *struct { i: int, z: size };
	assert(ptr.i == 42 && ptr.z == 1337);
};

fn indexing() void = {
	let x: ((int, uint), size) = ((42, 69), 1337);
	assert(x.0.0 == 42);
	assert(x.0.1 == 69);
	assert(x.1 == 1337);
	assert(x.1z == 1337);
	assert(x.0x1 == 1337);
	assert(x.1e+0 == 1337);
};

fn func(in: (int, size)) (int, size) = (in.0 + 1, in.1 + 1);
fn eval_expr_access() void = {
	static assert((42, 0).0 == 42 && (42, 0).1 == 0);
};

fn eval_expr_tuple() void = {
	static let t = (42, 8);
};

fn funcs() void = {
	let x = func((41, 1336));
	assert(x.0 == 42 && x.1 == 1337);
};

fn unpacking_static() int = {
	static let (a, b) = (0, 0);
	a += 1;
	b += 1;
	return a;
};

fn unpacking_demo() (int, int) = {
	return (10, 20);
};

fn unpacking_eval() (int, int) = {
	static let i = 0;
	const res = (10 + i, 20 + i);
	i += 1;
	return res;
};

let unpacking_global: int = 0i;

fn unpacking_addone() int = {
	unpacking_global += 1;
	return unpacking_global;
};

fn unpacking() void = {
	const (a, b, c) = (42, 8, 12);
	assert(a == 42);
	assert(b == 8);
	assert(c == 12);

	const (a, b): (i64, u64) = (2i, 4z);
	assert(a == 2i64);
	assert(b == 4u64);

	const (a, b, c): (i64, str, f64) = (2i, "hello", 1.0);
	assert(a == 2i64);
	assert(b == "hello");
	assert(c == 1.0f64);

	let (a, b): (i64, u64) = (1i, 3z);
	a += 1;
	b += 1;
	assert(a == 2i64);
	assert(b == 4u64);

	const (_, b, c) = (1, 2, 3);
	assert(b == 2);
	assert(c == 3);

	const (a, _, c) = (1, 2, 3);
	assert(a == 1);
	assert(c == 3);

	const (a, b, _) = (1, 2, 3);
	assert(a == 1);
	assert(b == 2);

	unpacking_static();
	unpacking_static();
	const a = unpacking_static();
	assert(a == 3);

	const (a, b) = unpacking_demo();
	assert(a == 10);
	assert(b == 20);

	const (a, b) = unpacking_eval();
	assert(a == 10);
	assert(b == 20);

	let (a, b, _, d) = (unpacking_addone(), unpacking_addone(),
		unpacking_addone(), unpacking_addone());
	assert(a == 1 && b == 2 && d == 4);

	assert(rt::compile("
		export fn main() void = {
			let (_, _) = (1, 2);
		};
	") != 0);
	assert(rt::compile("
		export fn main() void = {
			let (x, y) = (1, 2, 3);
		};
	") != 0);
	assert(rt::compile("
		export fn main() void = {
			let (x, y) = 5;
		};
	") != 0);
	assert(rt::compile("
		fn getval() int = 5;
		export fn main() void = {
			static let (a, b) = (2, getval());
		};
	") != 0);
	assert(rt::compile("
		fn getval() int = 5;
		export fn main() void = {
			static let (a, _) = (2, getval());
		};
	") != 0);
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
	unpacking();
	regression();
};
