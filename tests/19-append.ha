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

export fn main() void = {
	basics();
	multi();
	_static();
	withlength();
};
