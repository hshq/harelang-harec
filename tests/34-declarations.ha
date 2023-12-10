use rt::{compile, exited, EXIT_SUCCESS, toutf8};
use testmod;

def ARR: [3]u8 = [1, 2, 3];

// interdependent constants
def A1: int = -1;
def A2: int = A1 + 2;
def A3: [A2 + 2]int = [A1, A2, 0];
// reverse order
def B3: [B2 + 2]int = [B1, B2, 0];
def B2: int = B1 + 2;
def B1: int = -1;

// flexible types
def I = 12;
def F = 12.0;
def R = 'x';

// zero-size constants
def V = void;
def ZA: [0]int = [];
def ZS = struct { v: void = void };
def ZT = (void, void);

fn constants() void = {
	assert(ARR[0] == 1 && ARR[1] == 2 && ARR[2] == 3);
	assert(A1 == -1 && B1 == -1);
	assert(A2 == 1 && B2 == 1);
	assert(len(B3) == 3);
	assert(A3[0] == -1 && B3[0] == -1);
	assert(A3[1] == 1 && B3[1] == 1);
	assert(A3[2] == 0 && B3[2] == 0);

	let x = I;
	assert(x == 12);
	let x: *int = &x;
	let x: u8 = I;
	assert(x == 12);
	let x: i64 = I;
	assert(x == 12);

	let x = F;
	assert(x == 12.0);
	let x: *f64 = &x;
	let x: f32 = F;
	assert(x == 12.0);

	let x = R;
	assert(x == 'x');
	let x: *rune = &x;
	let x: u32 = R;
	assert(x == 'x');

	V;
	ZS;
	ZS.v;
	ZT;
	ZT.0;
	ZT.1;
	ZA;
	assert(len(ZA) == 0);
};

def C = 42;

fn local_constants() void = {
	def main = 42; // ensure hosted main check isn't used on locals
	static assert(main == 42);

	static assert(C == 42);
	def C = 0, C = 1337;
	static assert(C == 1337);
	{
		def C = 69;
		static assert(C == 69);
	};
	static assert(C == 1337);

	static let x = 0;
	def C = &x;
	assert(C == &x && *C == 0);

	let x = {
		def LEN = 4;
		yield [0...]: [LEN]int;
	};
	def LEN = 8;
	assert(len(x) == 4);

	let x: [LEN]int = {
		def LEN = 4;
		yield [0...];
	};
	assert(len(x) == LEN && LEN == 8);

	assert(compile(`fn test() void = {
		{ def X = 0; };
		def C = X;
	};`) as exited != EXIT_SUCCESS);
	assert(compile(`fn test() void = {
		{ def X = 0; };
		X;
	};`) as exited != EXIT_SUCCESS);
	assert(compile(`fn test() void = {
		let x = 0;
		def X = x;
	};`) as exited != EXIT_SUCCESS);
	assert(compile(`fn test() void = {
		let x = 0;
		def X = &x;
	};`) as exited != EXIT_SUCCESS);
	assert(compile(`fn test() void = {
		static let x = 0;
		def X = x;
	};`) as exited != EXIT_SUCCESS);
	assert(compile(`
		def X = 42;
		fn test() void = {
			static assert(X == 1337);
			def X = 42;
			static assert(X == 42);
		};
	`, "-DX=1337") as exited == EXIT_SUCCESS);
	assert(compile(`fn test() void = {
		static def X = 0;
	};`) as exited != EXIT_SUCCESS);
};

// elementary self-referential types
type self_slice = []self_slice;
type self_ptr = *self_ptr;
type self_slice_ptr = *[]self_slice_ptr;
type self_ptr_slice = []*self_ptr_slice;

// type referencing a constant
type arr1 = [sizearr1]str;
def sizearr1: size = 5z;
// reverse order
def sizearr2: size = 5z;
type arr2 = [sizearr2]str;

// self-referential struct
type struct1 = struct { a: []struct1 };

// self-referential struct with multiple indirections
type struct2 = struct { a: []**[]**nullable *struct2 };

// self-referential struct with self-reference having a nonzero offset
type struct3 = struct { a: int, data: *struct3 };

// struct with multiple self-refences
type struct4 = struct { ptr: *struct4, slice: []struct4, ptr2: *[]struct4 };


// self-referential indirect struct
type pstruct1 = []struct { a: pstruct1 };

// self-referential indirect struct with multiple indirections
type pstruct2 = []***[]struct { a: pstruct2 };

// self-referential indirect struct with self-reference having a nonzero offset
type pstruct3 = *struct { a: int, data: pstruct3 };

// indirect struct with multiple self-refences
type pstruct4 = *struct { a: pstruct4, b: [5]pstruct4, c: pstruct4 };


// self-referential tagged union
type tagged1 = (*tagged1 | void);

// self-referential tagged union with multiple indirections
type tagged2 = (***[][]nullable *tagged2 | int);

// tagged union with multiple self-references
type tagged3 = (*tagged3 | **tagged3 | []tagged3);

// tagged union with duplicate self-referential members
type tagged4 = (void | *tagged4 | int | *tagged4 | *tagged4 | str);


// self-referential indirect tagged union
type ptagged1 = *(ptagged1 | void);

// self-referential indirect tagged union with multiple indirections
type ptagged2 = []*nullable *[]*(ptagged2 | int);

// indirect tagged union with multiple self-references
type ptagged3 = *([2]ptagged3 | ptagged3 | (ptagged3, ptagged3));

// indirect tagged union with duplicate self-referential members
type ptagged4 = [](void | ptagged4 | int | ptagged4 | ptagged4 | str);


// self-referential tuple
type tuple1 = (*tuple1, u16);

// self-referential tuple with multiple indirections
type tuple2 = (***[][]nullable *tuple2, str);

// tuple with multiple self-references
type tuple3 = (*tuple3, *tuple3, []tuple3);


// self-referential indirect tuple
type ptuple1 = *(ptuple1, u16);

// self-referential indirect tuple with multiple indirections
type ptuple2 = ***[][]nullable *(ptuple2, str);

// tuple with multiple self-references
type ptuple3 = [](ptuple3, ptuple3, [3]ptuple3);


// elementary mutually recursive types
type mut_A1 = *mut_A2, mut_A2 = *mut_A1;

type mut_A3 = []mut_A4, mut_A4 = []mut_A3;

type mut_A5 = *mut_A6, mut_A6 = []mut_A5;

type mut_A7 = []mut_A8, mut_A8 = *mut_A7;

type mut_A9 = mut_A10, mut_A10 = *mut_A9;
type mut_B10 = *mut_B9, mut_B9 = mut_B10; // reverse

type mut_A11 = mut_A12, mut_A12 = []mut_A11;
type mut_B12 = []mut_B11, mut_B11 = mut_B12; // reverse

// mutually recursive structs
type mut_struct_A1 = struct { data: *mut_struct_A2 },
	mut_struct_A2 = struct { data: mut_struct_A1 };
type mut_struct_B2 = struct { data: mut_struct_B1 }, // reverse
	mut_struct_B1 = struct { data: *mut_struct_B2 };

// mutually recursive structs with padding
type mut_struct_A3 = struct { padding: u16, data: *mut_struct_A4 },
	mut_struct_A4 = struct { padding: u16, data: mut_struct_A3 };
type mut_struct_B4 = struct { padding: u16, data: mut_struct_B3 }, // reverse
	mut_struct_B3 = struct { padding: u16, data: *mut_struct_B4 };

// mutually recursive indirect structs
type mut_pstruct_A1 = *struct { data: mut_pstruct_A2 },
	mut_pstruct_A2 = struct { data: mut_pstruct_A1 };
type mut_pstruct_B2 = struct { data: mut_pstruct_B1 }, // reverse
	mut_pstruct_B1 = *struct { data: mut_pstruct_B2 };

// mutually recursive tagged unions
type mut_tagged_A1 = (*mut_tagged_A2 | u8), mut_tagged_A2 = (mut_tagged_A1 | u8);
type mut_tagged_B2 = (mut_tagged_B1 | u8), mut_tagged_B1 = (*mut_tagged_B2 | u8); // reverse

// mutually recursive tagged unions with repeated members
type mut_tagged_A3 = (*mut_tagged_A4 | u8 | *mut_tagged_A4),
	mut_tagged_A4 = (mut_tagged_A3 | u8 | mut_tagged_A3 | mut_tagged_A3);
type mut_tagged_B4 = (mut_tagged_B3 | u8 | mut_tagged_B3 | mut_tagged_B3), // reverse
	mut_tagged_B3 = (*mut_tagged_B4 | u8 | *mut_tagged_A4);

// mutually recursive indirect tagged unions
type mut_ptagged_A1 = *(mut_ptagged_A2 | u8), mut_ptagged_A2 = (mut_ptagged_A1 | u8);
type mut_ptagged_B2 = (mut_ptagged_B1 | u8), mut_ptagged_B1 = *(mut_ptagged_B2 | u8); // reverse

// mutually recursive tuples
type mut_tuple_A1 = (*mut_tuple_A2, u8), mut_tuple_A2 = (mut_tuple_A1, u8);
type mut_tuple_B2 = (mut_tuple_B1, u8), mut_tuple_B1 = (*mut_tuple_B2, u8); // reverse

// mutually recursive indirect tuples
type mut_ptuple_A1 = *(mut_ptuple_A2, u8), mut_ptuple_A2 = (mut_ptuple_A1, u8);
type mut_ptuple_B2 = (mut_ptuple_B1, u8), mut_ptuple_B1 = *(mut_ptuple_B2, u8); // reverse

// type with a type dimension dependency
type arri8_A = [size(arrintptr_A)]u8, arrintptr_A = [8]*int;
type arrintptr_B = [8]*int, arri8_B = [size(arrintptr_B)]u8; // reverse

type arri8_al_A = [align(arrintptr_al_A)]u8, arrintptr_al_A = [8]*int;
type arrintptr_al_B = [8]*int, arri8_al_B = [align(arrintptr_al_B)]u8; // reverse

type arr_circ = [size(*arr_circ)]int;

// mutually recursive types with a dimension dependency
type arru8_A = [size(arru8ptr_A)]u8, arru8ptr_A = [8]*arru8_A;
type arru8ptr_B = [8]*arru8_B, arru8_B = [size(arru8ptr_B)]u8; // reverse

type arru8_al_A = [align(arru8ptr_al_A)]u8, arru8ptr_al_A = [8]*arru8_al_A;
type arru8ptr_al_B = [8]*arru8_al_B, arru8_al_B = [align(arru8ptr_al_B)]u8; // reverse

// zero-size
let v = void;
let za: [0]int = [];
let zs = struct { v: void = void };
let zt = (void, void);

// unwrapped aliases to tagged unions
type unwrap_A1 = ([32]u8 | void | str),
	unwrap_A2 = (i64 | ...unwrap_A1),
	unwrap_alias_A1 = unwrap_A2,
	unwrap_alias_A2 = unwrap_alias_A1,
	unwrap_alias_A3 = ...unwrap_alias_A2;
type unwrap_alias_B3 = ...unwrap_alias_B2, // reverse
	unwrap_alias_B2 = unwrap_alias_B1,
	unwrap_alias_B1 = unwrap_B2,
	unwrap_B2 = (i64 | ...unwrap_B1),
	unwrap_B1 = ([32]u8 | void | str);

fn sz() void = {
	// size
	static assert(size(mut_struct_A3) == 2 * size(*opaque));
	static assert(size(mut_struct_B3) == 2 * size(*opaque));

	static assert(size(mut_tagged_A1) == 2 * size(*opaque));
	static assert(size(mut_tagged_B1) == 2 * size(*opaque));

	static assert(size(mut_tagged_A3) == 2 * size(*opaque));
	static assert(size(mut_tagged_B3) == 2 * size(*opaque));

	static assert(size(mut_tagged_A4) == 3 * size(*opaque));
	static assert(size(mut_tagged_B4) == 3 * size(*opaque));

	static assert(size(mut_tuple_A1) == 2 * size(*opaque));
	static assert(size(mut_tuple_B1) == 2 * size(*opaque));

	static assert(size(arru8_A) == 8 * size(*opaque));
	static assert(size(arru8_B) == 8 * size(*opaque));

	static assert(size(arru8ptr_A) == 8 * size(*opaque));
	static assert(size(arru8ptr_B) == 8 * size(*opaque));

	static assert(size(unwrap_A1) == size(unwrap_alias_A3));
	static assert(size(unwrap_B1) == size(unwrap_alias_B3));

	//align
	static assert(align(mut_struct_A3) == align(*opaque));
	static assert(align(mut_struct_B3) == align(*opaque));

	static assert(align(mut_tagged_A1) == align(*opaque));
	static assert(align(mut_tagged_B1) == align(*opaque));

	static assert(align(mut_tagged_A3) == align(*opaque));
	static assert(align(mut_tagged_B3) == align(*opaque));

	static assert(align(mut_tagged_A4) == align(*opaque));
	static assert(align(mut_tagged_B4) == align(*opaque));

	static assert(align(mut_tuple_A1) == align(*opaque));
	static assert(align(mut_tuple_B1) == align(*opaque));

	static assert(align(arru8_A) == align(u8));
	static assert(align(arru8_B) == align(u8));

	static assert(align(arru8ptr_A) == align(*opaque));
	static assert(align(arru8ptr_B) == align(*opaque));

	static assert(align(unwrap_A1) == align(unwrap_alias_A3));
	static assert(align(unwrap_B1) == align(unwrap_alias_B3));
};

fn hosted_main() void = {
	let pass = [
		"export fn main() void;",
		"export fn main() void = void;",
		`export @symbol("main") fn notmain() void;`,
		"fn main() void;",
		`export @symbol("notmain") fn main() int;`,
		`export let @symbol("notmain") notmain: int;`,
		"export fn not::main() int;",
	];

	for (let i = 0z; i < len(pass); i += 1) {
		assert(compile(pass[i]) as exited == EXIT_SUCCESS);
	};

	let failures = [
		"fn main() void = void;",
		"export fn main(x: int) void;",
		"export fn main() int;",
		"export type main = int;",
		"export def main = 0;",
		`export @symbol("main") fn f() int;`,
		"export let main: int;",
		`export let @symbol("main") notmain: int;`,
		"export type v = void; export fn main() v;",
	];

	for (let i = 0z; i < len(failures); i += 1) {
		assert(compile(failures[i]) as exited != EXIT_SUCCESS);
		assert(compile(failures[i], "-m\0", "\0") as exited == EXIT_SUCCESS);
	};

	assert(compile(`export @symbol(".main") fn main() int;`,
		"-m\0", ".main\0") as exited != EXIT_SUCCESS);
	assert(compile(`export @symbol("main") fn main() int;`,
		"-m\0", ".main\0") as exited == EXIT_SUCCESS);
	assert(compile(`export fn main() int;`,
		"-m\0", ".main\0") as exited != EXIT_SUCCESS);
};

fn reject() void = {
	let failures = [
		"type a = b; type b = a;",
		"type a = [20]a;",
		"type a = unknown;",
		"def x: int = 6; type a = x;",
		"type a = int; type a = str;",
		"def a: int = b; def b: int = a;",
		"def x: size = size(t); type t = [x]int;",
		"def a: int = 12; type t = (int |(...a | a));",
		"type a = (...unknown | int);",
		"def x = size(*foo);",
		"def x = size(*never);",
		"def x = size(*[*][*]int);",

		// usage of non-type aliases
		"let a: int = 4; type x = a;",
		"let a: int = 4; type x = *a;",
		"let a: int = 4; type x = []a;",
		"let a: int = 4; type x = [3]a;",
		"let a: int = 4; type x = (str, a);",
		"let a: int = 4; type x = (str | a);",
		"let a: int = 4; type x = struct { y: str, z: a};",
		"let a: int = 4; type x = union { y: str, z: a};",
		"let a: int = 4; fn x(y: str, z: a) void = { void; };",

		// binding not directly in compound expression
		"export fn main() void = let a = 4;",
		"export fn main() void = { if (true) let a = 4; };",

		// attributes on prototypes
		"@init fn f() void;",
		"@fini fn f() void;",
		"@test fn f() void;",

		// @symbol alongside other attributes
		`@symbol("foo") @init fn foo() void = void;`,
		`@symbol("foo") @fini fn foo() void = void;`,
		`@symbol("foo") @test fn foo() void = void;`,

		// initializing object with undefined size
		"let a: [*]int = [1, 2, 3];",

		// type alias of never
		"export type t = never;",

		// invalid symbol
		`@symbol("") fn f() void;`,
		`@symbol("#!@%") fn f() void;`,
		`@symbol("") let x: int;`,
		`@symbol("#!@%") let x: int;`,
	];

	for (let i = 0z; i < len(failures); i += 1) {
		assert(compile(failures[i]) as exited != EXIT_SUCCESS);
	};

	// usage of unexported type in exported declaration
	let unexported_type = [
		("a", "a { ... }"),
		("[4]a", "[a { ... }, a { ... }, a { ... }, a { ... }]"),
		("[]a", "[a { ... }]: []a"),
		("nullable *fn(x: a) void", "null: nullable *fn(x: a) void"),
		("nullable *fn() a", "null: nullable *fn() a"),
		("nullable *a", "null: nullable *a"),
		("struct { x: a }", "struct { x: a = a { ... } }"),
		("nullable *struct { a }", "null: nullable *struct { a }"),
		("nullable *union { x: a }", "null: nullable *union { x: a }"),
		("(int | a)", "a { ... }"),
		("(int, a)", "(0, a { ... })"),
	];
	let exported_decl = [
		("export type b = ", ";", ""),
		("export fn b(x: ", ") void = void;", ""),
		("export fn b() ", " = ", ";"),
		("export let b: ", ";", ""),
		("export let b = ", "", ";"),
		("export def b: ", " = ", ";"),
		("export def b = ", "", ";"),
	];
	let buf: [256]u8 = [0...];
	for (let i = 0z; i < len(unexported_type); i += 1) {
		for (let j = 0z; j < len(exported_decl); j += 1) {
			const t = unexported_type[i];
			const d = exported_decl[j];
			let buf = buf[..0];
			static append(buf, toutf8("type a = struct { x: int };\n")...);
			static append(buf, toutf8(d.0)...);
			if (d.1 != "") {
				static append(buf, toutf8(t.0)...);
				static append(buf, toutf8(d.1)...);
			};
			if (d.2 != "") {
				static append(buf, toutf8(t.1)...);
				static append(buf, toutf8(d.2)...);
			};
			assert(compile(*(&buf: *str)) as exited != EXIT_SUCCESS);
			static insert(buf[0], toutf8("export ")...);
			assert(compile(*(&buf: *str)) as exited == EXIT_SUCCESS);
		};
	};
};

// Types t_0 to t_9 form a complete directed graph on 10 vertices.
// The edge from t_$i to t_$j is indirect if $i > $j, otherwise it is direct.
// This ensures the generated graph is the maximum possible valid dependency
// graph of 10 hare type aliases.
type t_0 = (        t_1,  t_2,  t_3,  t_4,  t_5,  t_6,  t_7,  t_8,  t_9, );
type t_1 = ( *t_0,        t_2,  t_3,  t_4,  t_5,  t_6,  t_7,  t_8,  t_9, );
type t_2 = ( *t_0, *t_1,        t_3,  t_4,  t_5,  t_6,  t_7,  t_8,  t_9, );
type t_3 = ( *t_0, *t_1, *t_2,        t_4,  t_5,  t_6,  t_7,  t_8,  t_9, );
type t_4 = ( *t_0, *t_1, *t_2, *t_3,        t_5,  t_6,  t_7,  t_8,  t_9, );
type t_5 = ( *t_0, *t_1, *t_2, *t_3, *t_4,        t_6,  t_7,  t_8,  t_9, );
type t_6 = ( *t_0, *t_1, *t_2, *t_3, *t_4, *t_5,        t_7,  t_8,  t_9, );
type t_7 = ( *t_0, *t_1, *t_2, *t_3, *t_4, *t_5, *t_6,        t_8,  t_9, );
type t_8 = ( *t_0, *t_1, *t_2, *t_3, *t_4, *t_5, *t_6, *t_7,        t_9, );
type t_9 = ( *t_0, *t_1, *t_2, *t_3, *t_4, *t_5, *t_6, *t_7, *t_8,       );

fn complete_graph() void = {
	static assert(size(t_9) == 9 * size(*opaque));
	static assert(size(t_8) == 17 * size(*opaque));
	static assert(size(t_7) == 33 * size(*opaque));
	static assert(size(t_6) == 65 * size(*opaque));
	static assert(size(t_5) == 129 * size(*opaque));
	static assert(size(t_4) == 257 * size(*opaque));
	static assert(size(t_3) == 513 * size(*opaque));
	static assert(size(t_2) == 1025 * size(*opaque));
	static assert(size(t_1) == 2049 * size(*opaque));
	static assert(size(t_0) == 4097 * size(*opaque));
};

export let @symbol("s_y") s_c: int;

fn imported() void = {
	// Decl. with symbol accessible by identifier
	testmod::s_a;
	testmod::s_b;

	// Decl. with symbol not accessible by symbol
	assert(compile(`
	use testmod;
	fn test() void = {
		testmod::s_x;
	};`) as exited != EXIT_SUCCESS);
	assert(compile(`
	use testmod;
	fn test() void = {
		s_x;
	};`) as exited != EXIT_SUCCESS);

	// Decl. with same symbol are linked together
	assert(testmod::s_b == 1 && s_c == 1);
	s_c = 2;
	assert(testmod::s_b == 2 && s_c == 2);
};

export fn main() void = {
	constants();
	local_constants();
	sz();
	reject();
	imported();
	hosted_main();
	complete_graph();
};
