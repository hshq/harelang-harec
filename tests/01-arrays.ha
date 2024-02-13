use rt::{compile, status};

fn indexing() void = {
	let x = [1, 2, 3];
	let y = &x;
	let z = &y;
	assert(x[0] == 1 && x[1] == 2 && x[2] == 3);
	assert(y[0] == 1 && y[1] == 2 && y[2] == 3);
	assert(z[0] == 1 && z[1] == 2 && z[2] == 3);

	x[0] = 5;
	x[1] = 6;
	x[2] = 7;
	assert(x[0] == 5 && x[1] == 6 && x[2] == 7);
	assert(y[0] == 5 && y[1] == 6 && y[2] == 7);

	let q = &x[0];
	*q = 1337;
	assert(x[0] == 1337 && y[0] == 1337);

	compile(status::CHECK, "
		export fn main() void = {
			let a = [1, 2, 3];
			a[3];
		};
	")!;
};

type array_alias = [3]int;

fn measurements() void = {
	let x = [1, 2, 3];
	assert(len(x) == 3);
	assert(size([3]int) == size(int) * 3);
	assert(size([0]int) == 0);

	static assert(len(x) == 3);
	static assert(len(&x) == 3);
	static assert(len([1, 2, 3]) == 3);

	let y: array_alias = x;

	static assert(len(y) == 3);
	static assert(len(&y) == 3);

	assert(align([_]i8) == 1);
	assert(align([_]i16) == 2);
	assert(align([_]i32) == 4);
	assert(align([_]i64) == 8);
	assert(align([*]i8) == 1);
	assert(align([*]i16) == 2);
	assert(align([*]i32) == 4);
	assert(align([*]i64) == 8);
	assert(align([2]i8) == 1);
	assert(align([2]i16) == 2);
	assert(align([2]i32) == 4);
	assert(align([2]i64) == 8);
	assert(align([0]i32) == 4);
};

fn storage() void = {
	let x = [1, 2, 3];
	let y = &x: uintptr;
	assert(*((y + (size(int) * 0): uintptr): *int) == 1);
	assert(*((y + (size(int) * 1): uintptr): *int) == 2);
	assert(*((y + (size(int) * 2): uintptr): *int) == 3);
};

fn assignment() void = {
	let x = [1, 2, 3];
	let y = x;
	let z = [0, 0, 0];
	z = y;
	let w = [1, 0];
	w = [2, w[0]];
	assert(y[0] == 1 && y[1] == 2 && y[2] == 3);
	assert(z[0] == 1 && z[1] == 2 && z[2] == 3);
	assert(w[0] == 2 && w[1] == 1);
	let v: *[*]int = &x;

	compile(status::CHECK, "
		export fn main() void = {
			let a: [3]uint = [1u,2u,3u];
			let b: uint = 0;

			let ptr: *[3]uint = &a;
			ptr = &b;
		};
	")!;

	compile(status::CHECK, `
		export fn main() void = {
			let a: *[*]uint = &[1u,2u,3u];
			let b: [3]str = ["a", "b", "c"];
			a = &b;
		};
	`)!;

	compile(status::CHECK, `
		fn f() void = {
			let a = [1u, 2u, 3u];
			a += [3u, 2u, 1u];
		};
	`)!;
};

fn param(x: [3]int) void = {
	assert(len(x) == 3);
	assert(x[0] == 1);
	assert(x[1] == 2);
	assert(x[2] == 3);
};

fn nested() void = {
	let x = [[1, 2], [3, 4]];
	assert(x[0][0] == 1 && x[0][1] == 2);
	assert(x[1][0] == 3 && x[1][1] == 4);
	assert(len(x[0]) == 2);
	x[1] = [5, 6];
	assert(x[1][0] == 5 && x[1][1] == 6);
};

fn expanded() void = {
	let a: [5]int = [1337...];
	for (let i = 0z; i < len(a); i += 1) {
		assert(a[i] == 1337);
	};

	let b: [5]struct { x: int, y: int } = [struct {
		x: int = 10,
		y: int = 20,
	}...];
	for (let i = 0z; i < len(b); i += 1) {
		assert(b[i].x == 10 && b[i].y == 20);
	};

	let c: [5]int = [1, 2, 3...];
	let expected = [1, 2, 3, 3, 3];
	for (let i = 0z; i < len(c); i += 1) {
		assert(c[i] == expected[i]);
	};

	let q: [65535]int = [1, 2, 3...];
	let expected = [1, 2, 3];
	for (let i = 0z; i < len(expected); i += 1) {
		assert(q[i] == expected[i]);
	};
	for (let i = 3z; i < len(q); i += 1) {
		assert(q[i] == 3);
	};
};

fn extype() void = {
	let x: [5]u8 = [42...];
	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == 42);
	};
	x[0] = 24;
	assert(x[0] == 24);
	assert(x[1] == 42);
};

fn eval_array() void = {
	static let a = [1, 2];
};

fn eval_access() void = {
	static assert([1, 2][0] == 1 && [1, 2][1] == 2);
};

fn reject() void = {
	// unbounded arrays of values of undefined size
	compile(status::CHECK, "fn f() void = { let x = null: *[*][*]int; };")!;
	compile(status::CHECK, "fn f() void = { let x = null: *[*]fn ()int; };")!;

	// assignment to array of undefined size
	compile(status::CHECK, "let x: [*]int; fn f() void = { x = []; };")!;

	// compile-time evaluation of len with slice
	compile(status::CHECK, "let x: []int; let y = len(x);")!;
};

export fn main() void = {
	indexing();
	measurements();
	storage();
	assignment();
	param([1, 2, 3]);
	nested();
	expanded();
	extype();
	eval_array();
	eval_access();
	reject();
};
