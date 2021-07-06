use rt;

fn from_array() void = {
	let src = [1, 2, 3];
	let x: []int = src;
	let xptr = &x: *struct {
		data: nullable *[*]int,
		length: size,
		capacity: size,
	};
	assert(xptr.data == &src);

	let y: []int = [];
	let yptr = &y: *struct {
		data: nullable *[*]int,
		length: size,
		capacity: size,
	};
	assert(yptr.data == null);
};

fn storage() void = {
	let x: []int = [1, 2, 3, 4, 5];
	const expected = [1, 2, 3, 4, 5];

	let ptr = &x: *struct {
		data: *[*]int,
		length: size,
		capacity: size,
	};

	assert(len(x) == 5);
	assert(ptr.length == 5 && ptr.capacity == 5);

	for (let i = 0z; i < len(expected); i += 1) {
		assert(x[i] == expected[i]);
	};
};

fn casting() void = {
	let x: []int = [1, 2, 3, 4, 5];
	let y = x: *[5]int;
	for (let i = 0z; i < len(x); i += 1) {
		assert(x[i] == y[i]);
	};
};

fn measurements() void = {
	let x: []int = [1, 2, 3, 4, 5];
	assert(size([]int) == size(*[*]int) + size(size) * 2);
	assert(len(x) == 5);
	assert(&x: uintptr: size % size(int) == 0);
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

fn zero3(s: []int) void = {
	s[..] = [0, 0, 0];
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

	zero3(y);
	assert(y[0] == 0 && y[1] == 0 && y[2] == 0);
	let z: []int = [1, 2, 3, 4, 5];
	z[1..4] = [42, 69, 1337];
	assert(z[0] == 1 && z[1] == 42 && z[2] == 69 && z[3] == 1337 && z[4] == 5);
	z[2..5] = y;
	assert(z[0] == 1 && z[1] == 42 && z[2] == 0 && z[3] == 0 && z[4] == 0);
	assert(rt::compile(
		"export fn main() void = { let a: []int = [1]; a[..] += a; };"
	) != 0, "binop slice assignment");
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
	
	assert(rt::compile(
		"fn test() void = { let x = \"test\"; x[1..3]; };"
	) != 0, "slicing non-array, non-slice object");
	assert(rt::compile(
		"fn test() void = { let x = [1, 2, 3]; x[\"hi\"..]; };"
	) != 0, "slicing object with non-array, non-slice range");
};

type tree = struct {
	value: u64,
	children: []tree,
};

fn sum_tree(t: tree) u64 = {
 	let sum = t.value;
	for (let i = 0z; i < len(t.children); i += 1) {
		sum += sum_tree(t.children[i]);
	};
	return sum;
};

fn recursive_structure() void = {
	const t = tree {
		value = 15,
		children = [tree {
			value = 23,
			children = [tree {
				value = 62,
				children = [],
			}, tree {
				value = 34,
				children = [],
			}],
		}],
	};
	assert(sum_tree(t) == 134, "recursive structure using slices");
};

export fn main() void = {
	from_array();
	storage();
	measurements();
	indexing();
	assignment();
	slicing();
	recursive_structure();
};
