fn padding() void = {
	assert(size(struct { x: i32, y: i32 }) == 8z);
	assert(size(struct { x: i32, y: i64 }) == 16z);
	assert(size(union { x: i8, y: i16, z: i32 }) == 4z);
};

fn storage() void = {
	let coords = struct { x: i32 = 10i32, y: i32 = 20i32 };
	let ptr = &coords: *[*]i32;
	assert(ptr[0] == 10i32);
	assert(ptr[1] == 20i32);
};

export fn main() void = {
	padding();
	storage();
	// TODO: These tests need to be expanded when field selection works
};
