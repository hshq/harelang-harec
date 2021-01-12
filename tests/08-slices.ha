fn from_array() void = {
	let src = [1, 2, 3];
	let x: []int = src;
	let ptr = &x: *struct {
		data: *[*]int,
		length: size,
		capacity: size,
	};
	assert(ptr.data == &src);
};

fn storage() void = {
	let x: []int = [1, 2, 3, 4, 5];
	const expected = [1, 2, 3, 4, 5];

	let ptr = &x: *struct {
		data: *[*]int,
		length: size,
		capacity: size,
	};

	assert(len(x) == 5z);
	assert(ptr.length == 5z && ptr.capacity == 5z);

	for (let i = 0z; i < len(expected); i += 1z) {
		assert(x[i] == expected[i]);
	};
};

fn measurements() void = {
	let x: []int = [1, 2, 3, 4, 5];
	assert(size([]int) == size(*[*]int) + size(size) * 2z);
	assert(len(x) == 5z);
	assert(&x: uintptr: size % size(int) == 0z);
};

fn assignment() void = {
	let source = [1, 2, 3];
	let x: []int = source;
	x[0] = 4;
	x[1] = 5;
	x[2] = 6;
	assert(x[0] == 4 && x[1] == 5 && x[2] == 6);
	assert(source[0] == 4 && source[1] == 5 && source[2] == 6);
	let y: []int = [4, 5, 6];
	x = y;
	x[0] = 7;
	x[1] = 8;
	x[2] = 9;
	assert(x[0] == 7 && x[1] == 8 && x[2] == 9);
	assert(source[0] == 4 && source[1] == 5 && source[2] == 6);
};

export fn main() void = {
	from_array();
	storage();
	measurements();
	assignment();
};
