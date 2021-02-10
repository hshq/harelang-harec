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

export fn main() void = {
	storage();
	indexing();
};
