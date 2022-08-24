use rt::{compile, exited, EXIT_SUCCESS};

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
};

fn measurements() void = {
	let x = [1, 2, 3];
	assert(len(x) == 3);
	assert(size([3]int) == size(int) * 3);

	static assert(len([1, 2, 3]) == 3);
};

fn storage() void = {
	let x = [1, 2, 3];
	let y = &x: uintptr;
	assert(*((y + (size(int) * 0): uintptr): *int) == 1);
	assert(*((y + (size(int) * 1): uintptr): *int) == 2);
	assert(*((y + (size(int) * 2): uintptr): *int) == 3);
};

fn alignment() void = {
	let x: [_]i32 = [1, 2, 3];
	assert(&x: uintptr: size % 4 == 0);
	let y: [_]i64 = [1, 2, 3];
	assert(&y: uintptr: size % 8 == 0);
};

fn assignment() void = {
	let x = [1, 2, 3];
	let y = x;
	let z = [0, 0, 0];
	z = y;
	assert(y[0] == 1 && y[1] == 2 && y[2] == 3);
	assert(z[0] == 1 && z[1] == 2 && z[2] == 3);

	assert(compile("
		export fn main() void = {
			let a: [3]uint = [1u,2u,3u];
			let b: uint = 0;

			let ptr: *[3]uint = &a;
			ptr = &b;
		};
	") as exited != EXIT_SUCCESS);

	assert(compile(`
		export fn main() void = {
			let a: *[*]uint = &[1u,2u,3u];
			let b: [3]str = ["a", "b", "c"];
			a = &b;
		};
	`) as exited != EXIT_SUCCESS);
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

export fn main() void = {
	indexing();
	measurements();
	storage();
	alignment();
	assignment();
	param([1, 2, 3]);
	nested();
	expanded();
	extype();
	eval_array();
	eval_access();
};
