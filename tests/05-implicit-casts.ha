use rt::{compile, exited, EXIT_SUCCESS};

type subtype = struct {
	foo: int,
};

type super1 = struct {
	foo: subtype,
	bar: int,
};

type super2 = struct {
	subtype,
	bar: int,
};

type super3 = struct {
	struct { foo: int },
	bar: int,
};

type func = fn() void;

fn rules() void = {
	// Fixed precision ints
	let _i64: i64 = 0i64;
	_i64 = 42i8;
	_i64 = 42i16;
	_i64 = 42i32;
	_i64 = 42i;
	let _i32: i32 = 0i32;
	_i32 = 42i8;
	_i32 = 42i16;
	let _i16: i16 = 0i16;
	_i16 = 42i8;
	let _u64: u64 = 0u64;
	_u64 = 42u8;
	_u64 = 42u16;
	_u64 = 42u32;
	_u64 = 42u;
	let _u32: u32 = 0u32;
	_u32 = 42u8;
	_u32 = 42u16;
	let _u16: u16 = 0u16;
	_u16 = 42u8;

	// Implementation-defined precision
	if (size(int) == 8) {
		assert(compile("fn test() void = { let i: int = 42i64; };") as exited == EXIT_SUCCESS);
	};
	let i: int = 42i;
	i = 42i32;
	i = 42i16;
	i = 42i8;

	if (size(uint) == 8) {
		assert(compile("fn test() void = { let u: uint = 42u64; };") as exited == EXIT_SUCCESS);
	};
	let u: uint = 42u;
	u = 42u32;
	u = 42u16;
	u = 42u8;

	// Precision loss (should fail)
	assert(compile("fn test() void = { let _i8: i8 = 42i16; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _i8: i8 = 42i32; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _i8: i8 = 42i64; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _i8: i8 = 42i; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _i16: i16 = 42i32; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _i16: i16 = 42i64; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _i32: i32 = 42i64; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _u8: u8 = 42u16; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _u8: u8 = 42u32; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _u8: u8 = 42u64; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _u8: u8 = 42u; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _u16: u16 = 42u32; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _u16: u16 = 42u64; };") as exited != EXIT_SUCCESS);
	assert(compile("fn test() void = { let _u32: u32 = 42u64; };") as exited != EXIT_SUCCESS);

	// Pointer conversions
	let nptr: nullable *int = null;
	nptr = &i;
	let vptr: nullable *opaque = nptr;

	// Slice conversions
	let s: []int = [1, 2, 3];
	let s: []opaque = s;

	// Struct subtyping
	let sptr: *subtype = &super1 { ... };
	let sptr: *subtype = &super2 { ... };
	let sptr: *struct { foo: int } = &super3 { ... };

	// Invalid pointer conversions
	assert(compile(
		"fn test() void = { let x: nullable *int = null; let y: *int = x; };"
	) as exited != EXIT_SUCCESS);
	assert(compile(
		"fn test() void = { let x: int = 10; let y: *int = &x; let y: *uint = x; };"
	) as exited != EXIT_SUCCESS);
	assert(compile(
		"type i = int;"
		"fn test() void = { let x = &42; let y: *i = x; };"
	) as exited != EXIT_SUCCESS);
	assert(compile(
		"type v = void;"
		"fn test() void = { let x = &0; let x: *v = x; };"
	) as exited != EXIT_SUCCESS);

	// Invalid slice conversions
	assert(compile(
		"type i = int;"
		"fn test() void = { let x: []int = [1, 2, 3]; let y: []i = x; };"
	) as exited != EXIT_SUCCESS);
	assert(compile(
		"type v = void;"
		"fn test() void = { let x: []int = [0]; let x: []v = x; };"
	) as exited != EXIT_SUCCESS);

	// Non-const from const (copy)
	const j = 10;
	let k = j;
};

fn rvalue() i64 = {
	return 1234;
};

fn callme(in: i64) void = {
	assert(in == 1234i64);
};

fn calls() void = {
	callme(1234);
};

export fn main() void = {
	rules();
	assert(rvalue() == 1234i64);
	calls();
	// TODO: Expand this:
	// - Floats
	// - Arrays <-> slices
};
