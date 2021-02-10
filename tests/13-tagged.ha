use rt;

fn measurements() void = {
	const x: (u8 | u16 | u32 | u64) = 1337u16;	// With padding
	const align: size =
		if (size(u64) < size(uint)) size(uint)
		else size(u64);
	assert(size((u8 | u16 | u32 | u64)) == align * 2z);
	assert(&x: uintptr: size % size(uint) == 0z);
	assert(&x: uintptr: size % size(u64) == 0z);

	const y: (u8 | u16) = 1337u16;			// No padding
	assert(&y: uintptr: size % size(uint) == 0z);
	assert(&y: uintptr: size % size(u16) == 0z);
};

fn storage() void = {
	let x: (u8 | u16 | u32 | u64) = 42u8;
	const y = &x: *struct {
		tag: uint,
		union { _u8: u8, _u16: u16, _u32: u32, _u64: u64 },
	};
	assert(y.tag == 1228088861u); // u8 type ID
	assert(y._u8 == 42u8);

	x = 1337u16;
	assert(y.tag == 1225628936u); // u16 type ID
	assert(y._u16 == 1337u16);

	x = 0xCAFEBABEu32;
	assert(y.tag == 1226196611u); // u32 type ID
	assert(y._u32 == 0xCAFEBABEu32);

	x = 0xCAFEBABEDEADBEEFu64;
	assert(y.tag == 1226007386u); // u64 type ID
	assert(y._u64 == 0xCAFEBABEDEADBEEFu64);
};

fn operators() void = {
	let x: (u8 | u16 | u32 | u64) = 42u8;
	assert(x is u8);
	x = 1337u16;
	assert(x is u16);
	x = 0xCAFEBABEu32;
	assert(x is u32);
	x = 0xCAFEBABEDEADBEEFu64;
	assert(x is u64);
};

type signed = (i8 | i16 | i32 | i64 | int);
type unsigned = (u8 | u16 | u32 | u64 | uint);
type integer = (...signed | ...unsigned);

fn reduction() void = {
	const a: (i8 | i16) = 42i8;
	const b: (i16 | i8) = a;
	const c: (i8 | i16 | i32) = a;
	const d: (i8 | i16 | i8 | i16) = a;
	assert(rt::compile(
		// Cannot assign from more general type
		"fn test() void = {
			let a: (i8 | i16 | i32) = 42i8;
			let b: (i8 | i16) = a;
		};"
	) != 0);
	assert(a is i8 && b is i8 && c is i8 && d is i8);
	assert(size((i8 | i16 | i32)) == size((i8 | (i16 | i32))));
	assert(size(integer) == size(signed));
	assert(size(integer) != size((signed | unsigned)));
	const i: integer = 10i;
	assert(i is int);
};

fn casts() void = {
	let a: (u8 | u16) = 42u16;
	assert(a as u16 == 42u16);
	let x = a: u8;
	assert(x == 42u8);

	const val = 0xBEEFu16;
	const is_little = (&val: *[2]u8)[0] == 0xEFu8;
	a = 0xCAFEu16;
	x = a: u8;
	assert(x == (if (is_little) 0xFEu8 else 0xCAu8));
};

export fn main() void = {
	measurements();
	storage();
	operators();
	reduction();
	casts();
};
