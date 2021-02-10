fn storage() void = {
	let x: (int, size) = (42, 1337z);
	assert(size((int, size)) == size(size) * 2z);
	let ptr = &x: *struct { i: int, z: size };
	assert(ptr.i == 42 && ptr.z == 1337z);
};

fn indexing() void = {
	let x: (int, size) = (42, 1337z);
	assert(x.0 == 42 && x.1 == 1337z);
};

fn func(in: (int, size)) (int, size) = (in.0 + 1, in.1 + 1z);

fn funcs() void = {
	let x = func((41, 1336z));
	assert(x.0 == 42 && x.1 == 1337z);
};

export fn main() void = {
	storage();
	indexing();
	funcs();
};
