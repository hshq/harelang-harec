use rt;

type implicit_values = enum {
	ZERO,
	ONE,
	TWO,
};

fn implicit() void = {
	assert(implicit_values::ZERO == 0);
	assert(implicit_values::ONE == 1);
	assert(implicit_values::TWO == 2);
};

type explicit_values = enum {
	NEGONE = -1,
	SIXTEEN = 16,
	SEVENTEEN,
	EIGHTEEN,
	FIFTY = 50,
};

fn explicit() void = {
	assert(explicit_values::NEGONE == -1);
	assert(explicit_values::SIXTEEN == 16);
	assert(explicit_values::SEVENTEEN == 17);
	assert(explicit_values::EIGHTEEN == 18);
	assert(explicit_values::FIFTY == 50);
};

type with_storage = enum u16 {
	CAFE = 0xCAFE,
	BABE = 0xBABE,
	DEAD = 0xDEAD,
	BEEF = 0xBEEF,
};

type char_storage = enum char {
	FOO = 0,
	BAR = 1,
};

type rune_storage = enum rune {
	FOO = '0',
	BAR = '1',
};

fn storage() void = {
	assert(size(explicit_values) == size(int));
	assert(size(with_storage) == 2);
	const val = 0xBEEFu16;
	const is_little = (&val: *[2]u8)[0] == 0xEF;
	assert(with_storage::CAFE: u8 == (if (is_little) 0xFEu8 else 0xCAu8));
	assert(with_storage::BABE: u8 == (if (is_little) 0xBEu8 else 0xBAu8));
	assert(with_storage::DEAD: u8 == (if (is_little) 0xADu8 else 0xDEu8));
	assert(with_storage::BEEF: u8 == (if (is_little) 0xEFu8 else 0xBEu8));
	assert(char_storage::FOO == 0 && char_storage::BAR == 1);
	assert(rune_storage::FOO == '0' && rune_storage::BAR == '1');
};

fn reject() void = {
	assert(rt::compile("export let a: enum { A, B } = 0;") != 0);
	assert(rt::compile("export let a: int = 0: enum{A, B}: int;") != 0);
};

type interdependent1 = enum {
	A = 0,
	B = interdependent2::A + 1,
	C = interdependent2::B + 1,
};

type interdependent2 = enum {
	A = interdependent1::A + 1,
	B = interdependent1::B + 1,
	C = interdependent1::C + 1,
};

fn interdependent() void = {
	assert(interdependent1::A == 0);
	assert(interdependent2::A == 1);
	assert(interdependent1::B == 2);
	assert(interdependent2::B == 3);
	assert(interdependent1::C == 4);
	assert(interdependent2::C == 5);
};

export fn main() void = {
	implicit();
	explicit();
	storage();
	reject();
	interdependent();
};
