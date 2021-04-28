fn sleq(a: []int, b: []int) bool = {
	if (len(a) != len(b)) {
		return false;
	};
	for (let i = 0z; i < len(a); i += 1) {
		if (a[i] != b[i]) {
			return false;
		};
	};
	return true;
};

fn basics() void = {
	let x: []int = [];
	defer free(x);
	append(x, 1, 2, 3, 4, 5, 6);
	insert(x[2], 7, 8, 9);
	assert(sleq(x, [1, 2, 7, 8, 9, 3, 4, 5, 6]));

	let x: []int = alloc([], 9);
	defer free(x);
	append(x, 1, 2, 3, 4, 5, 6);
	insert(x[2], 7, 8, 9);
	assert(sleq(x, [1, 2, 7, 8, 9, 3, 4, 5, 6]));
};

export fn main() void = {
	basics();
	// TODO: Variadic insert
};
