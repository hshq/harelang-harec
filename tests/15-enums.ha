use rt::{compile, status};
use testmod;

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

type rune_storage = enum rune {
	FOO = '0',
	BAR = '1',
};

type uintptr_storage = enum uintptr {
	FOO = 0,
	BAR = 1,
};

let global_uintptr: uintptr_storage = uintptr_storage::FOO;

fn storage() void = {
	assert(size(explicit_values) == size(int));
	assert(size(with_storage) == size(u16));
	assert(size(uintptr_storage) == size(uintptr));
	assert(align(explicit_values) == align(int));
	assert(align(with_storage) == align(u16));
	assert(align(uintptr_storage) == align(uintptr));

	const val = 0xBEEFu16;
	const is_little = (&val: *[2]u8)[0] == 0xEF;
	assert(with_storage::CAFE: u8 == (if (is_little) 0xFEu8 else 0xCAu8));
	assert(with_storage::BABE: u8 == (if (is_little) 0xBEu8 else 0xBAu8));
	assert(with_storage::DEAD: u8 == (if (is_little) 0xADu8 else 0xDEu8));
	assert(with_storage::BEEF: u8 == (if (is_little) 0xEFu8 else 0xBEu8));
	assert(rune_storage::FOO == '0' && rune_storage::BAR == '1');
};

fn runes() void = {
	let val = '1': rune_storage;
	let val = rune_storage::FOO: rune;

	compile(status::CHECK, "type a = enum rune { A = 'x' }; fn f() void = { a::A: str; };")!;
};

fn reject() void = {
	// enum type definition used outside type declaration
	compile(status::PARSE, "export let a: enum { A, B } = 0;")!;
	compile(status::PARSE, "export let a: int = 0: enum{A, B}: int;")!;

	// enum circular dependencies
	compile(status::CHECK, "type a = enum { A = B, B = A };")!;
	compile(status::CHECK, "type a = enum { A = b::B }, b = enum { B = a::A };")!;
	compile(status::CHECK, "
		def a: int = e::VAL1;
		type e = enum { VAL1 = a };
	")!;
	compile(status::CHECK, "
		def a: int = e::VAL1;
		type e = enum { VAL1 = VAL2, VAL2 = a };
	")!;

	// invalid storage
	compile(status::PARSE, "type a = enum f64 { A = 0.0 };")!;

	// invalid value
	compile(status::CHECK, "type a = enum { A = void };")!;
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

type e1 = enum { a, b, c, d }, a1 = e1;
type a2 = e2, e2 = enum u8 { a, b, c, d }; // reverse

type imported_alias = testmod::_enum;

type imported_double_alias = testmod::enum_alias;

fn aliases() void = {
	assert(size(a1) == size(e1));
	assert(a1::a == e1::a);
	assert(a1::b == e1::b);
	assert(a1::c == e1::c);
	assert(a1::d == e1::d);

	assert(size(a2) == size(e2));
	assert(a2::a == e2::a);
	assert(a2::b == e2::b);
	assert(a2::c == e2::c);
	assert(a2::d == e2::d);

	// test with alias of imported enum
	assert(imported_alias::ONE == testmod::_enum::ONE);
	assert(imported_alias::TWO == testmod::_enum::TWO);
	assert(imported_alias::THREE == testmod::_enum::THREE);

	assert(imported_double_alias::ONE == testmod::_enum::ONE);
	assert(imported_double_alias::TWO == testmod::_enum::TWO);
	assert(imported_double_alias::THREE == testmod::_enum::THREE);

	// regression test: imported alias of enum where imported module's
	// namespace has multiple components
	assert(testmod::namespaced_alias::ONE == testmod::_enum::ONE);
	assert(testmod::namespaced_alias::TWO == testmod::_enum::TWO);
	assert(testmod::namespaced_alias::THREE == testmod::_enum::THREE);
};

// Force T2 to be resolved before t2::T3 and t2::T3 before t2::T1
type t1 = enum {
	I1 = t2::T1,
	I2 = t2::T3,
	I3 = T2: int,
};

def T2: uint = 0;

type t2 = enum {
	T1,
	T2 = 1,
	T3 = T2 + 1,
};

fn resolution_order() void = {
	assert(t2::T1 == 0);
	assert(t2::T2 == 1);
	assert(t2::T3 == 2);
	assert(t1::I1 == 0);
	assert(t1::I2 == 2);
	assert(t1::I3 == 0);
	assert(T2 == 0);
};

export fn main() void = {
	implicit();
	explicit();
	storage();
	runes();
	reject();
	interdependent();
	aliases();
	resolution_order();
};
