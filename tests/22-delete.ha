use rt;

type my_slice = []int;

fn index() void = {
	let x = alloc([]int, [1, 2, 3, 4, 5]);
	let y = &x: *my_slice;

	delete(x[1]);
	assert(len(x) == 4);
	assert(x[0] == 1 && x[1] == 3 && x[2] == 4 && x[3] == 5);

	delete(y[3]);
	assert(len(x) == 3);
	assert(x[0] == 1 && x[1] == 3 && x[2] == 4);
	free(x);
};

fn slice() void = {
	let x = alloc([]int, [1, 2, 3, 4, 5]): my_slice;
	let y = &x;
	const s = y: *rt::slice;

	delete(x[..3]);
	assert(len(x) == 2);
	assert(x[0] == 4 && x[1] == 5);

	delete(y[..]);
	assert(len(x) == 0);
	assert(s.capacity < 5);

	append(x, 6, 7, 8, 9);
	delete(x[1..3]);
	assert(len(x) == 2);
	assert(x[0] == 6 && x[1] == 9);
	free(x);
};

export fn main() void = {
	index();
	slice();
};
