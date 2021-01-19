type implicit_values = enum {
	ZERO,
	ONE,
	TWO,
};

fn implicit() void = {
	// TODO: Type promotion
	assert(implicit_values::ZERO == 0: implicit_values);
	assert(implicit_values::ONE == 1: implicit_values);
	assert(implicit_values::TWO == 2: implicit_values);
};

type explicit_values = enum {
	NEGONE = -1,
	SIXTEEN = 16,
	SEVENTEEN,
	EIGHTEEN,
	FIFTY = 50,
};

fn explicit() void = {
	// TODO: Type promotion
	assert(explicit_values::NEGONE == -1: explicit_values);
	assert(explicit_values::SIXTEEN == 16: explicit_values);
	assert(explicit_values::SEVENTEEN == 17: explicit_values);
	assert(explicit_values::EIGHTEEN == 18: explicit_values);
	assert(explicit_values::FIFTY == 50: explicit_values);
};

type with_storage = enum u16 {
	CAFE = 0xCAFEu16,
	BABE = 0xBABEu16,
	DEAD = 0xDEADu16,
	BEEF = 0xBEEFu16,
};

fn storage() void = {
	assert(size(explicit_values) == size(int));
	assert(size(with_storage) == 2z);
	const val = 0xBEEFu16;
	const is_little = (&val: *[2]u8)[0] == 0xEFu8;
	assert(with_storage::CAFE: u8 == (if (is_little) 0xFEu8 else 0xCAu8));
	assert(with_storage::BABE: u8 == (if (is_little) 0xBEu8 else 0xBAu8));
	assert(with_storage::DEAD: u8 == (if (is_little) 0xADu8 else 0xDEu8));
	assert(with_storage::BEEF: u8 == (if (is_little) 0xEFu8 else 0xBEu8));
};

export fn main() void = {
	implicit();
	explicit();
	storage();
};
