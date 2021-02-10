fn sizes() void = {
	// Fixed precision
	assert(size(i8) == 1);
	assert(size(i16) == 2);
	assert(size(i32) == 4);
	assert(size(i64) == 8);
	assert(size(u8) == 1);
	assert(size(u16) == 2);
	assert(size(u32) == 4);
	assert(size(u64) == 8);
	// Implementation-defined (test meets spec limits)
	assert(size(int) >= 4);
	assert(size(uint) >= 4);
	// etc
	assert(size(char) == 1);
};

fn alignment() void = {
	// Fixed alignment
	let _i8 = 0i8, _u8 = 0u8;
	assert(&_i8: uintptr: size % 1 == 0);
	assert(&_u8: uintptr: size % 1 == 0);
	let _i16 = 0i16, _u16 = 0u16;
	assert(&_i16: uintptr: size % 2 == 0);
	assert(&_u16: uintptr: size % 2 == 0);
	let _i32 = 0i32, _u32 = 0u32;
	assert(&_i32: uintptr: size % 4 == 0);
	assert(&_u32: uintptr: size % 4 == 0);
	let _i64 = 0i64, _u64 = 0u64;
	assert(&_i64: uintptr: size % 8 == 0);
	assert(&_u64: uintptr: size % 8 == 0);
	// Implementation-defined alignment
	let i = 0i, u = 0u, z = 0z, uptr = &i: uintptr;
	assert(&i: uintptr: size % size(int) == 0);
	assert(&u: uintptr: size % size(uint) == 0);
	assert(&z: uintptr: size % size(size) == 0);
	assert(&uptr: uintptr: size % size(uintptr) == 0);
};

export fn main() void = {
	sizes();
	alignment();
};
