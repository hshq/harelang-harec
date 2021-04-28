use rt;

fn simple() void = {
	let x: []int = alloc([1, 2, 3], 6);
	append(x, 4, 5, 6);
	assert(len(x) == 6);
	assert(x[0] == 1 && x[1] == 2 && x[2] == 3 && x[3] == 4 && x[4] == 5);
	assert(x[5] == 6);
	append(x, 7, 8, 9);
	assert(x[0] == 1 && x[1] == 2 && x[2] == 3 && x[3] == 4 && x[4] == 5);
	assert(x[5] == 6 && x[6] == 7 && x[7] == 8 && x[8] == 9);

	let y: [][]int = alloc([[1]], 1z);
	let z = [2, 3, 4];
	append(y, y...);
	append(y, z);
	assert(len(y) == 3);
	assert(len(y[0]) == 1 && len(y[1]) == 1 && len(y[2]) == 3);
	assert(y[0][0] == 1);
	assert(y[1][0] == 1);
	assert(y[2][0] == 2 && y[2][1] == 3 && y[2][2] == 4);
	free(x);
	free(y);
};

fn variadic() void = {
	let x: []int = alloc([1, 2], 2z);
	append(x, 4, [8, 16, 32, 64]...);
	assert(len(x) == 7);
	assert(x[0] == 1 && x[1] == 2 && x[2] == 4 && x[3] == 8 && x[4] == 16);
	assert(x[5] == 32 && x[6] == 64);

	let y: []int = alloc([1, 2, 3], 3);
	let z = [128, 256];
	append(y, x...);
	append(y, z...);
	assert(len(y) == 12);
	assert(y[0] == 1 && y[1] == 2 && y[2] == 3 && y[3] == 1 && y[4] == 2);
	assert(y[5] == 4 && y[6] == 8 && y[7] == 16 && y[8] == 32);
	assert(y[9] == 64 && y[10] == 128 && y[11] == 256);
	free(x);
	free(y);
};

fn static_append() void = {
	let buf: [4]int = [0...];
	let x = buf[..0];
	static append(x, 1, 2);
	static append(x, [3, 4]...);
	assert(x[0] == 1 && x[1] == 2 && x[2] == 3 && x[3] == 4);
	assert(buf[0] == 1 && buf[1] == 2 && buf[2] == 3 && buf[3] == 4);
};

export fn main() void = {
	simple();
	variadic();
	static_append();

	assert(rt::compile("fn test() void = append([1], 2);") != 0);
	assert(rt::compile("fn test() void = { let x: []int = alloc([1], 1z); append(x[..], 2); };") != 0);
};
