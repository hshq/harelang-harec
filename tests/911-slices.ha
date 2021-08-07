fn castarray() void = {
	let a: [_]int = [1, 2, 3];
	let x: []int = a;
	let p = &x: *struct {
		data: *[3]int,
		length: size,
		capacity: size,
	};
	assert(p.data == &a);
	assert(p.length == len(a));
	assert(p.capacity == len(a));
};

fn assert_slice_eq(actual: []int, expected: []int) void = {
	assert(len(expected) == len(actual));
	for (let i = 0z; i < len(expected); i += 1) {
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

	let p = &a;
	assert_slice_eq(p[..], [1, 2, 3, 4, 5]);
	assert_slice_eq(p[..3], [1, 2, 3]);
	assert_slice_eq(p[1..3], [2, 3]);
	assert_slice_eq(p[1..], [2, 3, 4, 5]);
};

export fn main() int = {
	castarray();
	slicing();
	// TODO: slice assignment
	return 0;
};
