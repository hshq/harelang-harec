export fn main() void = {
	assert(0xFFu8 << 8u16 == 0xFF00u16);
	assert(0xFFu8 << 8u8 == 0u8);
	assert(0xFFu8 << 24u32 == 0xFF000000u32);
	assert(0xFFu8 << 32u32 == 0u32);
	assert(0xFFu8 << 32u64 == 0xFF00000000u64);
//	assert(0xFFu8 << 64u64 == 0u64); TODO
};
