use rt::{compile, exited, EXIT_SUCCESS, toutf8};

type my_enum = enum u8 {
	FOO,
};

fn assignment() void = {
	let i = 0i8;
	let u = 0u64;
	let f = 0.0f64;
	let r = 'a';
	let e = my_enum::FOO;

	// There are five cases that need to be tested for tagged unions:
	// - The default type for the constant is a member of the union
	// - A single non-default type the constant could assume is a member of
	//   the union
	// - The default type for the constant along with at least one other
	//   type the constant could assume are both members of the union
	// - At least two types the constant could assume are members of the
	//   union, and the default type isn't a member of the union
	// - None of the types the constant could assume are members of the
	//   union
	// All but the fourth and fifth case are valid, and the invalid cases
	// should error out gracefully.
	let itu1: (int | void) = void;
	let itu2: (u64 | void) = void;
	let itu3: (int | u64 | void) = void;
	let ftu1: (f64 | void) = void;
	let ftu2: (f32 | void) = void;
	let ftu3: (f32 | f64 | void) = void;
	let rtu1: (rune | void) = void;
	let rtu2: (u64 | void) = void;
	let rtu3: (rune | u64 | void) = void;

	i = 127;
	assert(compile("export fn main() void = { let i = 0i8; i = 128; };") as exited != EXIT_SUCCESS);
	u = 18446744073709551615;
	assert(compile("export fn main() void = { let u = 0u32; u = 4294967296; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let f = 0.0f64; f = 0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let r = 'a'; r = 0; };") as exited != EXIT_SUCCESS);
	e = 0;
	assert(compile("type my_enum = enum u8 { FOO }; export fn main() void = { let e: my_enum = my_enum::FOO; e = 256; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let p: nullable *void = null; p = 0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let b = false; b = 0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let n = null; n = 0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let s: struct { i: int } = struct { i: int = 0 }; s = 0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let t = (0, 1); t = 0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let a = [0, 1]; a = 0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let s = \"\"; s = 0; };") as exited != EXIT_SUCCESS);
	itu1 = 0;
	itu2 = 0;
	itu3 = 0;
	assert(compile("export fn main() void = { let itu4: (u32 | u64 | void) = void; itu4 = 0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let itu5: (str | void) = void; itu5 = 0; };") as exited != EXIT_SUCCESS);

	assert(compile("export fn main() void = { let i = 0i8; i = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let u = 0u8; u = 0.0; };") as exited != EXIT_SUCCESS);
	f = 0.0;
	assert(compile("export fn main() void = { let r = 'a'; r = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("type my_enum = enum u8 { FOO }; export fn main() void = { let e: my_enum = my_enum::FOO; e = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let p: nullable *void = null; p = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let b = false; b = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let n = null; n = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let s: struct { i: int } = struct { i: int = 0 }; s = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let t = (0, 1); t = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let a = [0, 1]; a = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let s = ""; s = 0.0; };") as exited != EXIT_SUCCESS);
	ftu1 = 0.0;
	ftu2 = 0.0;
	ftu3 = 0.0;
	assert(compile("type my_f32 = f32; export fn main() void = { let ftu4: (f32 | my_f32 | void) = void; ftu4 = 0.0; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let ftu5: (str | void) = void; ftu5 = 0.0; };") as exited != EXIT_SUCCESS);

	i = 'a';
	u = 'a';
	assert(compile("export fn main() void = { let f = 0.0f64; f = 'a'; };") as exited != EXIT_SUCCESS);
	r = 'a';
	e = 'a';
	assert(compile("export fn main() void = { let p: nullable *void = null; p = 'a'; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let b = false; b = 'a'; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let n = null; n = 'a'; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let s: struct { i: int } = struct { i: int = 0 }; s = 'a'; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let t = (0, 1); t = 'a'; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let a = [0, 1]; a = 'a'; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let s = ""; s = 'a'; };") as exited != EXIT_SUCCESS);
	rtu1 = 'a';
	rtu2 = 'a';
	rtu3 = 'a';
	assert(rtu3 is rune);
	assert(compile("export fn main() void = { let rtu4: (u32 | u64 | void) = void; rtu4 = 'a'; };") as exited != EXIT_SUCCESS);
	assert(compile("export fn main() void = { let rtu5: (str | void) = void; rtu5 = 'a'; };") as exited != EXIT_SUCCESS);

	let u2: uint = 'a';
	let u2: uintptr = 'a';
	let z: size = 'a';
};

fn aggregates() void = {
	// Pointers

	// Kinda hacky way to verify that something has the expected type
	// The variables are necessary in order to avoid type hints, which would
	// avoid verifying that constants are lowered when entering aggregate
	// types
	let maxiptr = if (true) alloc(2147483647) else void;
	free(maxiptr as *int);
	let miniptr = if (true) alloc(-2147483648) else void;
	free(miniptr as *int);
	let smalli64ptr = if (true) alloc(2147483648) else void;
	free(smalli64ptr as *i64);
	let negi64ptr = if (true) alloc(-2147483649) else void;
	free(negi64ptr as *i64);
	let maxi64ptr = if (true) alloc(9223372036854775807) else void;
	free(maxi64ptr as *i64);
	// -9223372036854775808 can't be made to work without lots of hacks
	let mini64ptr = if (true) alloc(-9223372036854775807) else void;
	free(mini64ptr as *i64);
	let fptr = if (true) alloc(0.0) else void;
	free(fptr as *f64);
	let rptr = if (true) alloc('a') else void;
	free(rptr as *rune);

	// Tuples

	// The edge cases of the iconst lowering algorithm were already tested
	// above, and tuple items can't affect each other, so this suffices
	let tuple = if (true) (2147483647, 0.0, 'a') else void;
	tuple as (int, f64, rune);

	// Arrays
	let iarr = if (true) [0, 1, 2] else void;
	iarr as [3]int;
	let uarr = if (true) [0u8, 1, 2] else void;
	uarr as [3]u8;
	let u2arr = if (true) [0, 1u8, 2] else void;
	u2arr as [3]u8;
};

fn numeric() void = {
	let want: [_]i64 = [
		42, 42, 42, 42, 42, 42, 42, 42,
		0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1,
		100, 100, 100, 100
	];
	let i = [
		// basics
		(42, 42i, 42i8, 42i16, 42i32, 42i64), // decimal
		(42e0, 42e0i, 42e0i8, 42e0i16, 42e0i32, 42e0i64), // with exp
		(42e00, 42e00i, 42e00i8, 42e00i16, 42e00i32, 42e00i64), // with leading zeros in exp
		(42e+0, 42e+0i, 42e+0i8, 42e+0i16, 42e+0i32, 42e+0i64), // with + in exp
		(42e+00, 42e+00i, 42e+00i8, 42e+00i16, 42e+00i32, 42e+00i64), // with + and leading zeros in exp
		(0b101010, 0b101010i, 0b101010i8, 0b101010i16, 0b101010i32, 0b101010i64), // binary
		(0o52, 0o52i, 0o52i8, 0o52i16, 0o52i32, 0o52i64), // octal
		(0x2a, 0x2ai, 0x2ai8, 0x2ai16, 0x2ai32, 0x2ai64), // hex

		// single digit
		(0, 0i, 0i8, 0i16, 0i32, 0i64), // zero
		(0b0, 0b0i, 0b0i8, 0b0i16, 0b0i32, 0b0i64), // binary
		(0o0, 0o0i, 0o0i8, 0o0i16, 0o0i32, 0o0i64), // octal
		(0x0, 0x0i, 0x0i8, 0x0i16, 0x0i32, 0x0i64), // hex

		(1, 1i, 1i8, 1i16, 1i32, 1i64), // nonzero
		(0b1, 0b1i, 0b1i8, 0b1i16, 0b1i32, 0b1i64), // binary
		(0o1, 0o1i, 0o1i8, 0o1i16, 0o1i32, 0o1i64), // octal
		(0x1, 0x1i, 0x1i8, 0x1i16, 0x1i32, 0x1i64), // hex

		// with leading zero
		(0b00, 0b00i, 0b00i8, 0b00i16, 0b00i32, 0b00i64), // binary
		(0o00, 0o00i, 0o00i8, 0o00i16, 0o00i32, 0o00i64), // octal
		(0x00, 0x00i, 0x00i8, 0x00i16, 0x00i32, 0x00i64), // hex

		(0b01, 0b01i, 0b01i8, 0b01i16, 0b01i32, 0b01i64), // binary with leading zero
		(0o01, 0o01i, 0o01i8, 0o01i16, 0o01i32, 0o01i64), // octal
		(0x01, 0x01i, 0x01i8, 0x01i16, 0x01i32, 0x01i64), // hex

		// exponents
		(1e2, 1e2i, 1e2i8, 1e2i16, 1e2i32, 1e2i64),
		(1e02, 1e02i, 1e02i8, 1e02i16, 1e02i32, 1e02i64), // with leading zeros in exp
		(1e+2, 1e+2i, 1e+2i8, 1e+2i16, 1e+2i32, 1e+2i64), // with + in exp
		(1e+02, 1e+02i, 1e+02i8, 1e+02i16, 1e+02i32, 1e+02i64), // with + and leading zeros in exp
	];
	for (let j = 0z; j < len(i); j += 1) {
		let t = &i[j];
		assert(want[j] == t.0 && t.0 == t.1 && t.1 == t.2 && t.2 == t.3
			&& t.3 == t.4 && t.4 == t.5);
	};

	let u = [
		// basics
		(42z, 42u, 42u8, 42u16, 42u32, 42u64), // decimal
		(42e0z, 42e0u, 42e0u8, 42e0u16, 42e0u32, 42e0u64), // with exp
		(42e00z, 42e00u, 42e00u8, 42e00u16, 42e00u32, 42e00u64), // with leading zeros in exp
		(42e+0z, 42e+0u, 42e+0u8, 42e+0u16, 42e+0u32, 42e+0u64), // with + in exp
		(42e+00z, 42e+00u, 42e+00u8, 42e+00u16, 42e+00u32, 42e+00u64), // with + and leading zeros in exp
		(0b101010z, 0b101010u, 0b101010u8, 0b101010u16, 0b101010u32, 0b101010u64), // binary
		(0o52z, 0o52u, 0o52u8, 0o52u16, 0o52u32, 0o52u64), // octal
		(0x2az, 0x2au, 0x2au8, 0x2au16, 0x2au32, 0x2au64), // hex

		// single digit
		(0z, 0u, 0u8, 0u16, 0u32, 0u64), // zero
		(0b0z, 0b0u, 0b0u8, 0b0u16, 0b0u32, 0b0u64), // binary
		(0o0z, 0o0u, 0o0u8, 0o0u16, 0o0u32, 0o0u64), // octal
		(0x0z, 0x0u, 0x0u8, 0x0u16, 0x0u32, 0x0u64), // hex

		(1z, 1u, 1u8, 1u16, 1u32, 1u64), // nonzero
		(0b1z, 0b1u, 0b1u8, 0b1u16, 0b1u32, 0b1u64), // binary
		(0o1z, 0o1u, 0o1u8, 0o1u16, 0o1u32, 0o1u64), // octal
		(0x1z, 0x1u, 0x1u8, 0x1u16, 0x1u32, 0x1u64), // hex

		// with leading zero
		(0b00z, 0b00u, 0b00u8, 0b00u16, 0b00u32, 0b00u64), // binary
		(0o00z, 0o00u, 0o00u8, 0o00u16, 0o00u32, 0o00u64), // octal
		(0x00z, 0x00u, 0x00u8, 0x00u16, 0x00u32, 0x00u64), // hex

		(0b01z, 0b01u, 0b01u8, 0b01u16, 0b01u32, 0b01u64), // binary with leading zero
		(0o01z, 0o01u, 0o01u8, 0o01u16, 0o01u32, 0o01u64), // octal
		(0x01z, 0x01u, 0x01u8, 0x01u16, 0x01u32, 0x01u64), // hex

		// exponents
		(1e2z, 1e2u, 1e2u8, 1e2u16, 1e2u32, 1e2u64),
		(1e02z, 1e02u, 1e02u8, 1e02u16, 1e02u32, 1e02u64), // with leading zeros in exp
		(1e+2z, 1e+2u, 1e+2u8, 1e+2u16, 1e+2u32, 1e+2u64), // with + in exp
		(1e+02z, 1e+02u, 1e+02u8, 1e+02u16, 1e+02u32, 1e+02u64), // with + and leading zeros in exp
	];
	for (let j = 0z; j < len(u); j += 1) {
		let t = &u[j];
		assert(want[j]: u64 == t.0: u64 && t.0: u64 == t.1
			&& t.1 == t.2 && t.2 == t.3 && t.3 == t.4 && t.4 == t.5);
	};

	let f = [0.0, 0.00, 0.0e0, 0.00e0, 0.0e1, 0.00e1, 0.0e+0, 0.0e+1, 0.0e-0, 0.0e00,
		0.0e01, 0.0e+01, 0.0e+00, 0.0e-00, 0e-0, 0e-00, 0e-1, 0e-01];
	for (let j = 0z; j < len(f); j+= 1) {
		assert(f[j] == 0.0);
	};

	let _f32 = [0.0f32, 0.00f32, 0.0e0f32, 0.00e0f32, 0.0e1f32, 0.00e1f32, 0.0e+0f32,
		0.0e+1f32, 0.0e-0f32, 0.0e00f32, 0.0e01f32, 0.0e+01f32, 0.0e+00f32, 0.0e-00,
		0f32, 0e0f32, 0e1f32, 0e00f32, 0e01f32, 0e+0f32, 0e+00f32, 0e+1f32,
		0e+01f32, 0e-0f32, 0e-00f32, 0e-1f32, 0e-01f32];
	for (let j = 0z; j < len(_f32); j+= 1) {
		assert(_f32[j] == 0f32);
	};

	let _f64 = [0.0f64, 0.00f64, 0.0e0f64, 0.00e0f64, 0.0e1f64, 0.00e1f64, 0.0e+0f64,
		0.0e+1f64, 0.0e-0f64, 0.0e00f64, 0.0e01f64, 0.0e+01f64, 0.0e+00f64, 0.0e-00,
		0f64, 0e0f64, 0e1f64, 0e00f64, 0e01f64, 0e+0f64, 0e+00f64, 0e+1f64,
		0e+01f64, 0e-0f64, 0e-00f64, 0e-1f64, 0e-01f64];
	for (let j = 0z; j < len(_f64); j+= 1) {
		assert(_f64[j] == 0f64);
	};

	// double tuple subscript special case
	let tup = (('a', 'b'), 'c');
	assert(tup.0.0 == 'a');
	// exponents
	assert(tup.0e0.0 == 'a');
	assert(tup.0.0e0 == 'a');
	assert(tup.0e0.0e0 == 'a');
	assert(tup.0e+0.0 == 'a');
	assert(tup.0.0e+0 == 'a');
	assert(tup.0e+0.0e+0 == 'a');
	// signed
	assert(tup.0i.0 == 'a');
	assert(tup.0.0i == 'a');
	assert(tup.0i.0i == 'a');
	assert(tup.0i32.0 == 'a');
	assert(tup.0.0i32 == 'a');
	assert(tup.0i32.0i32 == 'a');
	// unsigned
	assert(tup.0u.0 == 'a');
	assert(tup.0.0u == 'a');
	assert(tup.0u.0u == 'a');
	assert(tup.0u32.0 == 'a');
	assert(tup.0.0u32 == 'a');
	assert(tup.0u32.0u32 == 'a');
	// bases
	assert(tup.0b0.0 == 'a');
	assert(tup.0.0b0 == 'a');
	assert(tup.0b0.0b0 == 'a');
	assert(tup.0o0.0 == 'a');
	assert(tup.0.0o0 == 'a');
	assert(tup.0o0.0o0 == 'a');
	assert(tup.0x0.0 == 'a');
	assert(tup.0.0x0 == 'a');
	assert(tup.0x0.0x0 == 'a');

	// zero with large exponent
	assert(0e10000000 == 0);
	assert(0e010000000 == 0);
	assert(0e+10000000 == 0);
	assert(0e+010000000 == 0);

	// f32 and f64 are valid hex literals
	assert(0xf32 == 3890);
	assert(0xf64 == 3940);
	assert(0x1f32 == 7986);
	assert(0x1f64 == 8036);
	assert(0xf321 == 62241);
	assert(0xf641 == 63041);


	// e is a valid hex digit
	assert(0xe == 14);
	assert(0xe+1 == 15);
	assert(0xe-1 == 13);
	assert(0x1e == 30);
	assert(0x1e+1 == 31);
	assert(0x1e-1 == 29);
	assert(0x1e1 == 481);
	assert(0x1e1f32 == 1974066);

	let v = if (true) 5else 10;
	assert(v == 5);

	let invalid: [_]str = [

		// invalid base
		"0b", "0o", // 0x tested separately
		"00b", "00o", "00x",
		"01b", "01o", "01x",
		"1b", "1o", "1x",
		"11b", "11o", "11x",

		// base with exponent
		"0b1e1",
		"0o1e1",
		// with +/-
		"0b1e+1",
		"0o1e+1",

		// invalid digits in smaller bases
		"0b41", "0b14",
		"0o82", "0o28",

		// leading zeroes
		"05", "00000010", "00.0", "01.0",
		"05e3", "00000010e3", "00.0e3", "01.0e3",
		"05e+3", "00000010e+3", "00.0e+3", "01.0e+3",
		"05e-3", "00000010e-3", "00.0e-3", "01.0e-3",

		// invalid sequences of special characters
		"1.",
		"1..",
		"1..1",
		"1.1.",
		"1.1.1",

		"1e",
		"1e+",
		"1e-",

		"1e1+",
		"1e1-",

		"1ee",
		"1e+e", "1ee+", "1e+e+",
		"1e-e", "1ee-", "1e-e-",
		"1e+e-", "1e-e+",

		"1ee1",
		"1e+e1", "1ee+1", "1e+e+1",
		"1e-e1", "1ee-1", "1e-e-1",
		"1e+e-1", "1e-e+1",

		"1e1e",
		"1e+1e", "1e1e+", "1e+1e+",
		"1e-1e", "1e1e-", "1e-1e-",
		"1e+1e-", "1e-1e+",

		"1e1e1",
		"1e+1e1", "1e1e+1", "1e+1e+1",
		"1e-1e1", "1e1e-1", "1e-1e-1",
		"1e+1e-1", "1e-1e+1",

		"1.e", "1e.",
		"1.e1", "1e.1",
		"1.1e", "1e1.",
		"1e1.1",

		"1.e+", "1e+.",
		"1.e+1", "1e+.1",
		"1.1e+", "1e+1.",
		"1e+1.1",

		"1.e-", "1e-.",
		"1.e-1", "1e-.1",
		"1.1e-", "1e-1.",
		"1e-1.1",
	];
	let extra: [_]str = [
		"4e-0i;", "4e-1i;",
		"4e-0i8;", "4e-1i8;",

		"0b1e-1f32;",
		"0o1e-1f32;",
		"0x1e+1f32;",
		"0x1e-1f32;",

		// exponent overflow
		"let t: u64 = 1e1000;",
	];
	let suffix = [";", "i;", "i8;", "f32;"];
	let buf: [256]u8 = [0...];
	for (let i = 0z; i < len(invalid); i += 1) {
		for (let j = 0z; j < len(suffix); j += 1) {
			let buf = buf[..0];
			append(buf, toutf8("let t = ")...);
			append(buf, toutf8(invalid[i])...);
			append(buf, toutf8(suffix[j])...);
			assert(compile(*(&buf: *str)) as exited != EXIT_SUCCESS);
		};
	};
	for (let i = 0z; i < len(extra); i += 1) {
		assert(compile(extra[i]) as exited != EXIT_SUCCESS);
	};
};

fn basics() void = {
	let b1 = true, b2 = false;
	let p1: nullable *int = null;
	let r1 = ['x', '\x0A', '\u1234', '\0', '\a', '\b', '\f', '\n', '\r', '\t',
		'\v', '\\', '\'', '\"', '\U12345678'];
};

export fn main() void = {
	// The interaction between constants and result type reduction is tested
	// in 30-reduction.c
	basics();
	numeric();
	assignment();
	aggregates();
};
