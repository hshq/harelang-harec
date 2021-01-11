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
	assert(ptr.length == 5z);
	assert(ptr.capacity == 5z);

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

export fn main() void = {
	from_array();
	storage();
	measurements();
};
