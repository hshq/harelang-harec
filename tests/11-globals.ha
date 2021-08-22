use rt;

let x: int = 42, y: int = 69;

fn write() void = {
	assert(x == 42 && y == 69);
	x = 1337;
	assert(x == 1337);
};

let ar: [3]int = [1, 2, 3];
let sl: []int = [1, 2, 3];
let st: str = "Hello!";

type coords = struct { x: int, y: int };
let su: coords = coords { y = 10, x = 20 };
let au: coords = coords { ... };

type coords3 = struct { coords: coords, z: int };
let a3: coords3 = coords3 { ... };

type aenum = enum u64 {
	BIG_VALUE = 0x1234567887654321,
};
const big_value: aenum = aenum::BIG_VALUE;

const float: f32 = 1234.5678;
const double: f64 = 1234.5678;

fn storage() void = {
	assert(len(ar) == 3);
	assert(ar[0] == 1 && ar[1] == 2 && ar[2] == 3);
	assert(len(sl) == 3);
	assert(sl[0] == 1 && sl[1] == 2 && sl[2] == 3);
	assert(len(st) == 6);
	assert(su.x == 20 && su.y == 10);
	assert(au.x == 0 && au.y == 0);
	assert(a3.coords.x == 0 && a3.coords.y == 0 && a3.z == 0);
	assert(big_value == 0x1234567887654321);
	assert(float == 1234.5678);
	assert(double == 1234.5678);
};

fn invariants() void = {
	assert(rt::compile("fn test() int; let x: int = test();") != 0);
	assert(rt::compile("const a: u8 = 2; const b: u8 = a + 5;") != 0);
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
const tup: (u8, str) = (2, "asdf");
const ptr: *u32 = &val;
const ptr_tup: *(u8, str) = &tup;
// TODO const ptr_memb: *str = &tup.1;

fn pointers() void = {
	assert(ptr == &val && *ptr == val);
};

type foo = (int | uint);

let subtype: foo = 10u;

fn tagged() void = {
	assert(subtype is uint);
	// TODO: subset-compat
};

export fn main() void = {
	// TODO: Expand this test:
	// - Declare & validate globals of more types
	// - Globals which are pointers to other globals
	write();
	storage();
	invariants();
	static_binding();
	pointers();
};
