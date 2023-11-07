use rt::{compile, exited, EXIT_SUCCESS};

fn basics() void = {
	let x: []int = [];
	append(x, 1);
	append(x, 2);
	append(x, 3);
	assert(len(x) == 3);
	assert(x[0] == 1 && x[1] == 2 && x[2] == 3);
	free(x);
};

fn multi() void = {
	let x: []int = [];
	append(x, [1, 2, 3]...);
	assert(len(x) == 3);

	let y: []int = [4, 5, 6];
	append(x, y...);
	assert(len(x) == 6);

	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == i: int + 1);
	};

	free(x);

	let x: []int = alloc([], 3);
	append(x, &[1, 2, 3]...);
	assert(len(x) == 3);
	free(x);
};

fn _static() void = {
	let buf: [32]int = [0...];
	let x = buf[..0];
	static append(x, 1);
	static append(x, 2);
	static append(x, 3);
	assert(len(x) == 3);

	static append(x, [4, 5, 6]...);
	assert(len(x) == 6);

	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == i: int + 1);
		assert(buf[i] == i: int + 1);
	};

	let x = [1, 2, 3][..0];
	static append(x, &[1, 2, 3]...);
	assert(len(x) == 3);
};

fn withlength() void = {
	let x: []int = [];
	append(x, [42...], 10);

	assert(len(x) == 10);
	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == 42);
	};

	free(x);
};

fn typehints() void = {
	let x: []u8 = [];
	append(x, 42);
	append(x, [42]...);
	append(x, [42...], 3);
	assert(len(x) == 5);
	for (let i = 0z; i < 5; i += 1) {
		assert(x[i] == 42u8);
	};
	free(x);
};

fn reject() void = {
	assert(compile("
		let x: []u8 = [0u8];
		let y: int = 42;
		append(x, y);
	") as exited != EXIT_SUCCESS); // object member type != value type
	assert(compile("
		let x: []u8 = [0u8];
		let y = 42u8;
		append(x, y...);
	") as exited != EXIT_SUCCESS); // value is not an array or a slice
	assert(compile("
		let x: []u8 = [0u8];
		let y: []int = [42];
		append(x, y...);
	") as exited != EXIT_SUCCESS); // object member type != value member type
	assert(compile("
		let x: []u8 = [0u8];
		append(x, [42i...], 5);
	") as exited != EXIT_SUCCESS); // same as above, but for an expression with length
	assert(compile("
		let x: []u8 = [0u8];
		append(x, [0u8...], 2i);
	") as exited != EXIT_SUCCESS); // length expression is not assignable to size
	assert(compile("
		let x: []u8 = [0u8];
		append(x, [42], 3);
	") as exited != EXIT_SUCCESS); // must be an expandable array
	assert(compile("
		let x: []u8 = [0u8];
		let x: nullable *[]u8 = &x;
		append(x, 42);
	") as exited != EXIT_SUCCESS); // object member type is nullable pointer
};

fn _never() void = {
	let x: []int = [];
	{ append(x, yield); };
	{ append(x, [0...], yield); };
};

export fn main() void = {
	basics();
	multi();
	_static();
	withlength();
	typehints();
	reject();
	_never();
};
