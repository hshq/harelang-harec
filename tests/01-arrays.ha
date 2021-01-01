fn indexing() void = {
	let x = [1, 2, 3];
	let y = &x;
	let z = &y;
	assert(x[0] == 1);
	assert(x[1] == 2);
	assert(x[2] == 3);
	assert(y[0] == 1);
	assert(y[1] == 2);
	assert(y[2] == 3);
	assert(z[0] == 1);
	assert(z[1] == 2);
	assert(z[2] == 3);
};

fn measurements() void = {
	let x = [1, 2, 3];
	assert(len(x) == 3z);
	assert(size([3]int) == size(int) * 3z);
};

fn storage() void = {
	let x = [1, 2, 3];
	let y = &x: uintptr;
	assert(*((y + (size(int) * 0z): uintptr): *int) == 1);
	assert(*((y + (size(int) * 1z): uintptr): *int) == 2);
	assert(*((y + (size(int) * 2z): uintptr): *int) == 3);
};

fn alignment() void = {
	let x = [1i32, 2i32, 3i32];
	assert(&x: uintptr: size % 4z == 0z);
	let y = [1i64, 2i64, 3i64];
	assert(&y: uintptr: size % 8z == 0z);
};

fn assignment() void = {
	let x = [1, 2, 3];
	let y = x;
	let z = [0, 0, 0];
	z = y;
	assert(y[0] == 1);
	assert(y[1] == 2);
	assert(y[2] == 3);
	assert(z[0] == 1);
	assert(z[1] == 2);
	assert(z[2] == 3);
};

fn param(x: [3]int) void = {
	assert(len(x) == 3z);
	assert(x[0] == 1);
	assert(x[1] == 2);
	assert(x[2] == 3);
};

fn nested() void = {
	let x = [[1, 2], [3, 4]];
	assert(x[0][0] == 1);
	assert(x[0][1] == 2);
	assert(x[1][0] == 3);
	assert(x[1][1] == 4);
	assert(len(x[0]) == 2z);
	// TODO: Assignment to nested array
	// x[1] = [5, 6];
};

export fn main() void = {
	// TODO: Assign to index, take address of index
	indexing();
	measurements();
	storage();
	alignment();
	assignment();
	param([1, 2, 3]);
	nested();
};
