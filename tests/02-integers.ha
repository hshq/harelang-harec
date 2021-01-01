fn sizes() void = {
	// Fixed precision
	assert(size(i8) == 1z);
	assert(size(i16) == 2z);
	assert(size(i32) == 4z);
	assert(size(i64) == 8z);
	assert(size(u8) == 1z);
	assert(size(u16) == 2z);
	assert(size(u32) == 4z);
	assert(size(u64) == 8z);
	// Implementation-defined (test meets spec limits)
	assert(size(int) >= 4z);
	assert(size(uint) >= 4z);
	// etc
	assert(size(char) == 1z);
};

fn alignment() void = {
	// Fixed alignment
	let _i8 = 0i8, _u8 = 0u8;
	assert(&_i8: uintptr: size % 1z == 0z);
	assert(&_u8: uintptr: size % 1z == 0z);
	let _i16 = 0i16, _u16 = 0u16;
	assert(&_i16: uintptr: size % 2z == 0z);
	assert(&_u16: uintptr: size % 2z == 0z);
	let _i32 = 0i32, _u32 = 0u32;
	assert(&_i32: uintptr: size % 4z == 0z);
	assert(&_u32: uintptr: size % 4z == 0z);
	let _i64 = 0i64, _u64 = 0u64;
	assert(&_i64: uintptr: size % 8z == 0z);
	assert(&_u64: uintptr: size % 8z == 0z);
	// Implementation-defined alignment
	let i = 0i, u = 0u, z = 0z, uptr = &i: uintptr;
	assert(&i: uintptr: size % size(int) == 0z);
	assert(&u: uintptr: size % size(uint) == 0z);
	assert(&z: uintptr: size % size(size) == 0z);
	assert(&uptr: uintptr: size % size(uintptr) == 0z);
};

export fn main() void = {
	sizes();
	alignment();
};
