fn castarray() void = {
	let a: [_]int = [1, 2, 3];
	let x: []int = a;
	let p = &x: *struct {
		data: *[3]int,
		length: size,
		capacity: size,
	};
	assert(p.data == &a);
	assert(p.length == len(a));
	assert(p.capacity == len(a));
};

export fn main() int = {
	castarray();
	// TODO: slicing, indexing, slice assignment
	return 0;
};
