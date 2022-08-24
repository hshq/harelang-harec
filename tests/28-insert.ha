use rt::{compile, exited, EXIT_SUCCESS};

fn basics() void = {
	let x: []int = alloc([1, 2, 5]);
	let y = &x;
	insert(x[2], 4);
	insert(x[2], 3);
	assert(len(x) == 5);
	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == i: int + 1);
	};
	insert(y[0], 42);
	assert(len(x) == 6 && x[0] == 42);
	free(x);
};

fn multi() void = {
	let x: []int = alloc([1, 2, 5]);
	insert(x[2], [3, 4]...);
	assert(len(x) == 5);
	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == i: int + 1);
	};
	free(x);

	let x: []int = alloc([1, 2, 5]);
	let y: []int = [3, 4];
	insert(x[2], y...);
	assert(len(x) == 5);
	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == i: int + 1);
	};
	free(x);
};

fn _static() void = {
	let buf: [32]int = [1, 2, 5, 42...];
	let x = buf[..3];
	static insert(x[2], 4);
	static insert(x[2], 3);
	assert(len(x) == 5);
	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == i: int + 1);
		assert(buf[i] == i: int + 1);
	};
	let z: []int = [1, 2, 3, 4];
	static insert(x[2], [1, 2, 3, 4]...);
	static insert(x[2], z...);
	for (let i = len(x); i < len(buf); i += 1) {
		assert(buf[i] == 42);
	};
};

fn typehints() void = {
	let x: []u8 = [];
	insert(x[0], 42);
	insert(x[1], [42]...);
	assert(len(x) == 2);
	assert(x[0] == 42u8);
	assert(x[1] == 42u8);
	free(x);
};

fn reject() void = {
	assert(compile("
		let x: []u8 = [0u8];
		let y: int = 42;
		insert(x[1], y);
	") as exited != EXIT_SUCCESS); // object member type != value type
	assert(compile("
		let x: []u8 = [0u8];
		let y = 42u8;
		insert(x[1], y...);
	") as exited != EXIT_SUCCESS); // value is not an array or a slice
	assert(compile("
		let x: []u8 = [0u8];
		let y: []int = [42];
		insert(x[1], y...);
	") as exited != EXIT_SUCCESS); // object member type != value member type
	assert(compile("
		let x: []u8 = [0u8];
		let y: []int = [42];
		insert(x[1], [y...], 3);
	") as exited != EXIT_SUCCESS); // insert expression doesn't have a length parameter
};

export fn main() void = {
	basics();
	multi();
	_static();
	typehints();
	reject();
};
