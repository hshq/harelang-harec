fn basics() void = {
	let x = 0z;
	for (let i = 0z; i < 10; i += 1) {
		assert(i == x);
		x += 1;
	};
	assert(x == 10);

	let x = 0z;
	for (let i = 0z; i < 10) {
		assert(i == x);
		x += 1;
		i += 1;
	};
	assert(x == 10);

	let x = 0z;
	for (x < 10; x += 1) void;
	assert(x == 10);
};

export fn main() int = {
	basics();
	// TODO: test flow control
	return 0;
};
