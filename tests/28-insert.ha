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
	let y = &x;
	insert(y[2], 7, 8, 9);
	assert(sleq(x, [1, 2, 7, 8, 9, 3, 4, 5, 6]));
};

fn variadic() void = {
	let x: []int = [];
	defer free(x);
	append(x, 1, 2, 3, 4, 5, 6);
	insert(x[2], [7, 8, 9]...);
	assert(sleq(x, [1, 2, 7, 8, 9, 3, 4, 5, 6]));

	let x: []int = [];
	defer free(x);
	let y = &x;
	let z = &y;
	append(y, 1, 2, 3, 4, 5, 6);
	insert(z[2], 7, 8, 9, [10, 11, 12]...);
	assert(sleq(x, [1, 2, 7, 8, 9, 10, 11, 12, 3, 4, 5, 6]));
};

fn static_insert() void = {
	let buf: [4]int = [0...];
	let x = buf[..0];
	static insert(x[0], 1, 2);
	static insert(x[1], [3, 4]...);
	assert(x[0] == 1 && x[1] == 3 && x[2] == 4 && x[3] == 2);
	assert(buf[0] == 1 && buf[1] == 3 && buf[2] == 4 && buf[3] == 2);
};

export fn main() void = {
	basics();
	variadic();
	static_insert();
};
