fn measurements() void = {
	let x: (u8 | u16 | u32 | u64) = 1337u16;
	assert(size((u8 | u16 | u32 | u64)) == size(u64) + size(size));
	const align: size =
		if (size(u64) < size(size)) size(size)
		else size(u64);
	assert(&x: uintptr: size % align == 0z);
};

fn storage() void = {
	let x: (u8 | u16 | u32 | u64) = 42u8;
	let y = &x: *struct {
		tag: size,
		union { _u8: u8, _u16: u16, _u32: u32, _u64: u64 },
	};
	assert(y.tag == 605989269682102909z); // u8 type ID
	assert(y._u8 == 42u8);

	x = 1337u16;
	assert(y.tag == 593553793169496424z); // u16 type ID
	assert(y._u16 == 1337u16);

	x = 0xCAFEBABEu32;
	assert(y.tag == 596423518518559459z); // u32 type ID
	assert(y._u32 == 0xCAFEBABEu32);

	x = 0xCAFEBABEDEADBEEFu64;
	assert(y.tag == 595466943402205114z); // u64 type ID
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

fn reduction() void = {
	let a: (i8 | i16) = 42i8;
	let b: (i16 | i8) = a;
	let c: (i8 | i16 | i32) = a;
	assert(a is i8);
	assert(b is i8);
	assert(c is i8);
	assert(size((i8 | i16 | i32)) == size((i8 | (i16 | i32))));
};

export fn main() void = {
	measurements();
	storage();
	operators();
	reduction();
	// TODO: Expand this as other tagged union features become available
};
