fn dimensions() void = {
	// Fixed precision
	assert(size(i8) == 1 && align(i8) == 1 && size(i8) % align(i8) == 0);
	assert(size(i16) == 2 && align(i16) == 2 && size(i16) % align(i16) == 0);
	assert(size(i32) == 4 && align(i32) == 4 && size(i32) % align(i32) == 0);
	assert(size(i64) == 8 && align(i64) == 8 && size(i64) % align(i64) == 0);
	assert(size(u8) == 1 && align(u8) == 1 && size(u8) % align(u8) == 0);
	assert(size(u16) == 2 && align(u16) == 2 && size(u16) % align(u16) == 0);
	assert(size(u32) == 4 && align(u32) == 4 && size(u32) % align(u32) == 0);
	assert(size(u64) == 8 && align(u64) == 8 && size(u64) % align(u64) == 0);
	// Implementation-defined (test meets spec limits)
	assert(size(int) >= 4 && align(int) >= 4 && size(int) % align(int) == 0);
	assert(size(uint) >= 4 && align(uint) >= 4 && size(uint) % align(uint) == 0);
	assert(size(size) % align(size) == 0);
	assert(size(uintptr) % align(uintptr) == 0);
	// etc
	assert(size(char) == 1 && align(char) == 1 && size(char) % align(char) == 0);
	assert(size(rune) == 4 && align(rune) == 4 && size(rune) % align(rune) == 0);
};

export fn main() void = {
	dimensions();
};
