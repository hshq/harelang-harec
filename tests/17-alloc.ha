type my_struct = struct {
	x: int,
	y: int,
};

type my_struct_ptr = *my_struct;

fn allocation() void = {
	let x = alloc(*int, 1234);
	assert(*x == 1234);
	free(x);

	let y = alloc(nullable *int, 1234);
	// TODO: Type promotion
	if (y != null: nullable *int) {
		assert(*(y: *int) == 1234);
	};
	free(y);

	let z = alloc(my_struct_ptr, struct {
		x: int = 42,
		y: int = 69,
	});
	assert(z.x == 42 && z.y == 69);
	free(z);
};

fn assignment() void = {
	let x = alloc(*int, 1234);
	*x = 4321;
	assert(*x == 4321);
	free(x);
};

fn double_pointer() void = {
	let x = alloc(*int, 1234);
	let y = alloc(**int, x);
	*x = 4321;
	assert(**y == 4321);
	**y = 1337;
	assert(*x == 1337);
	free(y);
	free(x);
};

fn double_alloc() void = {
	let x = alloc(*int, 1234);
	let y = alloc(*int, 4321);
	assert(x != y && *x != *y);
	free(x);
	free(y);
};

fn slice() void = {
	let x = alloc([]int, [1, 2, 3], 10z);
	assert(len(x) == 3z);
	for (let i = 0z; i < len(x); i += 1z) {
		assert(x[i] == (i + 1z): int);
	};
	free(x);
};

fn string() void = {
	let x = struct {
		data: *[3]int = alloc(*[3]int, [1, 2, 3]),
		length: size = 3z,
		capacity: size = 3z,
	};
	let y = *(&x: *str);
	assert(len(y) == 3z);
	free(y);
};

export fn main() void = {
	assignment();
	allocation();
	double_pointer();
	double_alloc();
	slice();
};
