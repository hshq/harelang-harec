fn rt::compile(src: str) int;

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

fn indexing() void = {
	let x = [1, 3, 3, 7];
	assert(x[0] == 1 && x[1] == 3 && x[2] == 3 && x[3] == 7);
	assert(rt::compile(
		"fn test() void = { let x: []int = [1, 2, 3]; x[\"hello\"]; };"
	) != 0, "non-numeric index");
	assert(rt::compile(
		"fn test() void = { let x = 10; x[10]; };"
	) != 0, "indexing non-array, non-slice object");
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

fn assert_slice_eq(actual: []int, expected: []int) void = {
	assert(len(expected) == len(actual));
	for (let i = 0z; i < len(expected); i += 1z) {
		assert(expected[i] == actual[i]);
	};
};

fn slicing() void = {
	let a = [1, 2, 3, 4, 5];
	assert_slice_eq(a[..], [1, 2, 3, 4, 5]);
	assert_slice_eq(a[..3], [1, 2, 3]);
	assert_slice_eq(a[1..3], [2, 3]);
	assert_slice_eq(a[1..], [2, 3, 4, 5]);

	let b: []int = [1, 2, 3, 4, 5];
	assert_slice_eq(b[..], [1, 2, 3, 4, 5]);
	assert_slice_eq(b[..3], [1, 2, 3]);
	assert_slice_eq(b[1..3], [2, 3]);
	assert_slice_eq(b[1..], [2, 3, 4, 5]);
	
	assert(rt::compile(
		"fn test() void = { let x = \"test\"; x[1..3]; };"
	) != 0, "slicing non-array, non-slice object");
	assert(rt::compile(
		"fn test() void = { let x = [1, 2, 3]; x[\"hi\"..]; };"
	) != 0, "slicing object with non-array, non-slice range");
};

export fn main() void = {
	from_array();
	storage();
	measurements();
	indexing();
	assignment();
	slicing();
};
