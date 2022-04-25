use rt;

type my_slice = []int;

fn index() void = {
	let x: []int = alloc([1, 2, 3, 4, 5]);
	let y = &x: *my_slice;

	delete(x[1]);
	assert(len(x) == 4);
	assert(x[0] == 1 && x[1] == 3 && x[2] == 4 && x[3] == 5);

	static delete(y[3]);
	assert(len(x) == 3);
	delete(y[0]);
	assert(len(x) == 2);
	assert(x[0] == 3 && x[1] == 4);
	free(x);
};

fn slice() void = {
	let x: my_slice = alloc([1, 2, 3, 4, 5]);
	let y = &x;
	const s = y: *rt::slice;

	delete(x[..3]);
	assert(len(x) == 2);
	assert(x[0] == 4 && x[1] == 5);

	delete(x[len(x)..]);

	static delete(y[..]);
	assert(len(x) == 0);
	assert(s.capacity < 5);

	append(x, [6, 7, 8, 9]...);
	delete(x[1..3]);
	assert(len(x) == 2);
	assert(x[0] == 6 && x[1] == 9);
	free(x);
};

export fn main() void = {
	index();
	slice();
};
