use rt::{compile, exited, EXIT_SUCCESS};

// globals without type hint
def NOHINT = 1234z;
let nohint = 12z;
const nohintconst = 123z;
// with type defined afterwards
const nhval = nh {
	field = 1,
};
type nh = struct {
	field: int,
};

let x: int = 42, y: int = 69;

let v0: void = void;
let v1 = void;
let v2: [0]int = [];
let v3 = (void, void);
let v4 = struct { v: void = void };
def V0: void = void;
def V1 = void;
def V2: [0]int = [];
def V3 = (void, void);
def V4 = struct { v: void = void };

fn write() void = {
	assert(x == 42 && y == 69);
	x = 1337;
	assert(x == 1337);

	assert(nohint == 12);
	assert(nohintconst == 123);
	assert(NOHINT == 1234);
	assert(nhval.field == 1);
};

let ar: [3]int = [1, 2, 3];
let sl: []int = [1, 2, 3];
let st: str = "Hello!";

type coords = struct { x: int, y: int };
let su: coords = coords { y = 10, x = 20};
let su_autofill: coords = coords { y = 10, x = 20, ... };
let au: coords = coords { ... };

type coords3 = struct { coords: coords, z: int };
let a3: coords3 = coords3 { ... };

type embedded = struct { a: uint, b: u8 };

type with_embedded = struct { embedded, c: int };
let em: with_embedded = with_embedded { a = 3, b = 4, c = 18 };
let em_autofill: with_embedded = with_embedded { ... };

type with_embedded2 = struct { c: int, embedded };
let em2: with_embedded2 = with_embedded2 { a = 3, b = 4, c = 18 };
let em2_autofill: with_embedded2 = with_embedded2 { ... };

type aenum = enum u64 {
	BIG_VALUE = 0x1234567887654321,
};
const big_value: aenum = aenum::BIG_VALUE;

type renum = enum rune {
	R1 = 'a',
	R2 = 'g',
};
let renum_val = renum::R1;

const float: f32 = 1234.5678;
const double: f64 = 1234.5678;

fn storage() void = {
	assert(len(ar) == 3);
	assert(ar[0] == 1 && ar[1] == 2 && ar[2] == 3);
	assert(len(sl) == 3);
	assert(sl[0] == 1 && sl[1] == 2 && sl[2] == 3);
	assert(len(st) == 6);
	assert(su.x == 20 && su.y == 10);
	assert(su_autofill.x == 20 && su_autofill.y == 10);
	assert(au.x == 0 && au.y == 0);
	assert(a3.coords.x == 0 && a3.coords.y == 0 && a3.z == 0);
	assert(em.a == 3 && em.b == 4 && em.c == 18);
	assert(em_autofill.a == 0 && em_autofill.b == 0 && em_autofill.c == 0);
	assert(em2.a == 3 && em2.b == 4 && em2.c == 18);
	assert(em2_autofill.a == 0 && em2_autofill.b == 0 && em2_autofill.c == 0);
	assert(big_value == 0x1234567887654321: aenum);
	assert(float == 1234.5678);
	assert(double == 1234.5678);
};

fn invalid() void = {
	assert(compile("fn test() int; let x: int = test();") as exited != EXIT_SUCCESS);
	assert(compile("const a: u8 = 2; const b: u8 = a + 5;") as exited != EXIT_SUCCESS);

	assert(compile("def a;") as exited != EXIT_SUCCESS);
	assert(compile("def a: int;") as exited != EXIT_SUCCESS);
	assert(compile("let a;") as exited != EXIT_SUCCESS);

	assert(compile("def a: [_]str = 0;") as exited != EXIT_SUCCESS);
	assert(compile("def a: int = \"string\";") as exited != EXIT_SUCCESS);
	assert(compile("def a: [_]int = [\"string\"];") as exited != EXIT_SUCCESS);
	assert(compile("def a: [_]int = [1, 2];") as exited == EXIT_SUCCESS);

	assert(compile("let a: [_]str = 0;") as exited != EXIT_SUCCESS);
	assert(compile("let a: int = \"string\";") as exited != EXIT_SUCCESS);
	assert(compile("let a: [_]int = [\"string\"];") as exited != EXIT_SUCCESS);
	assert(compile("let a: [_]int = [1, 2];") as exited == EXIT_SUCCESS);

	assert(compile("let a = [];") as exited != EXIT_SUCCESS);
	assert(compile("def a = [];") as exited != EXIT_SUCCESS);
};

fn counter() int = {
	static let x = 0;
	x += 1;
	return x;
};

fn static_binding() void = {
	assert(counter() == 1);
	assert(counter() == 2);
	assert(counter() == 3);
};

const val: u32 = 42;
const arr: [3]int = [1, 2, 3];
const _struct = struct { x: u32 = 1, y: u64 = 2 };
const tup: (u8, str) = (2, "asdf");
const ptr: *u32 = &val;
const ptr_arr: *int = &arr[1];
const ptr_struct = &_struct.y;
const ptr_tup: *str = &tup.1;

fn pointers() void = {
	assert(ptr == &val && *ptr == val);
	assert(ptr_arr == &arr[1] && *ptr_arr == 2);
	assert(ptr_struct == &_struct.y && *ptr_struct == 2);
	assert(ptr_tup == &tup.1 && *ptr_tup == "asdf");

	assert(compile("let a = [1, 2, 3]; let b = &a[3];") as exited != EXIT_SUCCESS);
	assert(compile("let a = (1, 2, 3); let b = &a.3;") as exited != EXIT_SUCCESS);
};

type foo = (int | uint);

let subtype: foo = 10u;

let arr_of_tagged_of_tuple: [3]((int, int)|void) = [(1, 2), (3, 4), (5, 6)];

type align4 = (i32 | u32);
type align8 = (i32 | u32 | u64);
let tagged4: align4 = 10u32;
let tagged8: align8 = 10u32;
let tagged8_u64: align8 = 10u64;

fn tagged() void = {
	assert((arr_of_tagged_of_tuple[0] as (int, int)).0 == 1);
	assert((arr_of_tagged_of_tuple[0] as (int, int)).1 == 2);
	assert((arr_of_tagged_of_tuple[1] as (int, int)).0 == 3);
	assert((arr_of_tagged_of_tuple[1] as (int, int)).1 == 4);
	assert((arr_of_tagged_of_tuple[2] as (int, int)).0 == 5);
	assert((arr_of_tagged_of_tuple[2] as (int, int)).1 == 6);
	assert(subtype is uint);
	// TODO: subset-compat

	let t4 = &tagged4: *struct {
		tag: u32,
		@offset(4) union {
			_i32: i32,
			_u32: u32,
		},
	};
	assert(t4.tag == 3481467866); // u32 type ID
	assert(t4._u32 == 10);

	let t8 = &tagged8: *struct {
		tag: u32,
		@offset(4) union {
			_i32: i32,
			_u32: u32,
		},
		@offset(8) _u64: u64,
	};
	assert(t8.tag == 3481467866); // u32 type ID
	assert(t8._u32 == 10);

	let t8 = &tagged8_u64: *struct {
		tag: u32,
		@offset(4) union {
			_i32: i32,
			_u32: u32,
		},
		@offset(8) _u64: u64,
	};
	assert(t8.tag == 1906196061); // u64 type ID
	assert(t8._u64 == 10);
};

// Real-world sample

type basic = enum {
	FN,
	FOR,
	IF,
	IN,
	NOT,
	SWITCH,
	WHILE,
};

const keywords: [_](str, basic) = [
	("fn", basic::FN),
	("for", basic::FOR),
	("if", basic::IF),
	("in", basic::IN),
	("not", basic::NOT),
	("switch", basic::SWITCH),
	("while", basic::WHILE),
];

fn tuplearray() void = {
	assert(keywords[0].0 == "fn");
	assert(keywords[1].0 == "for");
	assert(keywords[2].0 == "if");
	assert(keywords[3].0 == "in");
	assert(keywords[4].0 == "not");
	assert(keywords[5].0 == "switch");
	assert(keywords[6].0 == "while");
};

export fn main() void = {
	// TODO: Expand this test:
	// - Declare & validate globals of more types
	// - Globals which are pointers to other globals
	write();
	storage();
	invalid();
	static_binding();
	pointers();
	tagged();
	tuplearray();
};
