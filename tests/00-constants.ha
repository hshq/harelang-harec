use rt::{compile, exited, EXIT_SUCCESS};

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

export fn main() void = {
	let i1 = 13, i2 = 13i, i3 = 13i8, i4 = 13i16, i5 = 13i32, i6 = 13i64;
	let u1 = 13u, u2 = 13z, u3 = 13u8, u4 = 13u16, u5 = 13u32, u6 = 13u64;
	let n1 = -13, n2 = -13u;
	let b1 = true, b2 = false;
	let p1: nullable *int = null;
	let r1 = 'x', r2 = '\x0A', r3 = '\u1234', r4 = '\0', r5 = '\a',
		r6 = '\b', r7 = '\f', r8 = '\n', r9 = '\r', r10 = '\t',
		r11 = '\v', r12 = '\\', r13 = '\'', r14 = '\"',
		r15 = '\U12345678';
	let f1 = 1.0, f2 = 1f32, f3 = 1.0e2, f4 = 1.0f64;
	let f5 = 1.23e+45, f6 = 9.87e-65, f7 = 1e-7, f8 = 5.0e-324;

	// The interaction between constants and result type reduction is tested
	// in 30-reduction.c
	assignment();
	aggregates();
};
