// Miscellaneous regression tests
use rt;
use rt::{compile, status};

type embedded = struct {
	a: u64,
	b: u8,
};

type thing = struct {
	offs: u64,
	e: embedded,
};

def THING: thing = thing{
	offs = 0,
	e = embedded {
		a = 1,
		b = 0,
	},
};

type mod::t = (size | const size); // needs to have a type id larger than size

// order of these three matters
type b = struct { c: c };
type a = struct { b };
type c = *a;

// order of these two matters
let packedsz = size(packed);
type packed = struct @packed { a: u64, b: u8 };

type d = [3]int;
type e = bool;
let x = [1, 2, 3]: d: []int;
static assert(true: e == true: e);
static assert('a' == 'a');

static assert(~0u8 == 0xffu8);
static assert(-2u8 == 0xfeu8);

fn control_never() void = {
	let x = {
		yield yield 0;
	};
	assert(x == 0);
	x = {
		return yield 1;
	};
	assert(x == 1);
	x = {
		let x: int = yield 2;
		abort();
	};
	assert(x == 2);
	{
		for (true; yield) x += 1;
	};
	assert(x == 3);
	{
		for (let i: int = yield; true) x += 1;
	};
	assert(x == 3);
	{
		x = yield;
		x = 4;
	};
	assert(x == 3);
	{
		x += yield;
		abort();
	};
	assert({
		let x = true;
		x &&= yield x;
		abort();
	});

	if (false) {
		let x: int = abort();
		x += abort();
		let x = true;
		x &&= abort();
	};

	control_never1() as int;
	control_never2() as int;
};

fn control_never1() (int | void) = {
	return return 0;
};

fn control_never2() (int | void) = {
	{
		yield return 0;
	};
};

export fn main() void = {
	let t = thing {
		offs = 0,
		e = embedded {
			a = 1,
			b = 0,
		},
	};
	let t = t;
	assert(t.e.a == 1);

	let t2 = THING;
	assert(t2.e.a == 1);

	t2.offs = 42;
	assert(THING.offs == 0);

	let x: (void | int) = 10;
	match (x) {
	case let i: int =>
		assert(i == 10);
	case void =>
		abort();
	};

	let p = 0;
	let p = &p: uintptr: u64: (u64 | void);
	let p = match (p) {
	case void =>
		abort();
	case let p: u64 =>
		yield p: uintptr: *int;
	};
	assert(*p == 0);

	let thing: int = 0;
	let thing = &thing: (*int | int);
	let p = match (thing) {
	case int =>
		abort();
	case let p: *int =>
		yield p;
	};
	*p = 0;

	match (void: (void | !void)) {
	case void => void;
	case !void => abort();
	};

	let s: []f64 = [1.0, 2.0, 3.0];
	s[..] = [0.0...];

	compile(status::CHECK, "
		fn a() void = switch (b) {
		case &c => void;
		};"
	)!;

	compile(status::PARSE, "let a;")!;
	compile(status::CHECK, "
		type a = struct {
			b: int,
			c: int,
		};
		def A: a = a { b = 0 };"
	)!;
	compile(status::CHECK, "let a = &0;")!;
	compile(status::CHECK, "def A: a = 1 % 1;")!;
	compile(status::CHECK, "def A: b = void;")!;
	static assert(true == true && true != false);
	compile(status::CHECK, "
		type a = str;
		type b = struct { a };
		def A = b { c = 0 };"
	)!;
	compile(status::CHECK, "
		def A = 0;
		fn a() void = A = 0;"
	)!;
	compile(status::CHECK, "def A = x && true;")!;
	compile(status::CHECK, "type a = struct { b: fn() void };")!;
	compile(status::CHECK, "fn a() []int = alloc([]: [*]int, 0);")!;
	compile(status::CHECK, "fn a() [1]int = [1]: []int: [1]int;")!;
	compile(status::CHECK, "fn a() void = &*&a;")!;
	compile(status::CHECK, "let a = [*&0];")!;
	compile(status::CHECK, "fn a() *opaque = alloc(void);")!;
	compile(status::CHECK, "fn a() void = { static let b = x & struct { a: int = 0 }; };")!;
	let a: (size | mod::t) = 0;
	assert(0xffu8 + 1 >> 1 == 0);
	compile(status::CHECK, "type a = *...b; type b = *...a;")!;
	compile(status::CHECK, "def A = len([1]: []str);")!;

	control_never();
	compile(status::CHECK, "fn a() void = { abort(): int; };")!;

	// identifier exceeds maximum length
	let buf: [1024]u8 = [0...];
	let buf = buf[..0];
	static append(buf, rt::toutf8("let a")...);
	// IDENT_MAX (in identifier.h) is defined as 255
	for (let i = 0z; i < 255 / 2; i += 1) {
		static append(buf, rt::toutf8("::a")...);
	};
	const n = len(buf);
	static append(buf, rt::toutf8(" = 0;")...);
	compile(status::SUCCESS, *(&buf: *str))!;
	static insert(buf[n], rt::toutf8("::a")...);
	compile(status::PARSE, *(&buf: *str))!;
	assert(size(packed) == packedsz);
	assert(size(packed) == 9);

	let r = 'a';
	r: i64;

	:outer {
		let x: []int = {
			if (true) yield :outer;
			yield [];
		};
		abort();
	};

	compile(status::CHECK, "fn f() void = { let x = 0; x { ... }; };")!;

	compile(status::CHECK, "static assert(size(b) == 0); let b = 0;")!;

	assert((if (false) 0) is void);
	let v = if (false) 0;
	assert(v is void);
	assert((if (true) 0) is int);
	v = if (true) 0;
	assert(v is int);

	compile(status::CHECK, "fn a() void = { offset(a.b); };")!;

	compile(status::CHECK, "fn f() void = { let x: []u8 = &0: *[*]u8; };")!;

	compile(status::CHECK, "fn f(x: t = 2u) void = void;")!;

	compile(status::CHECK, "fn f() void = { for (let i = return; true) void; };")!;

	compile(status::CHECK, "fn f(a: (int | uint) = invalid) void = void;")!;

	compile(status::CHECK, "def X = void: done;")!;

	// tuple indexing lexer thing: even after a comment, 0.0 should be lexed
	// as three separate tokens, rather than as a float literal
	assert(((1, 2), 3). // don't remove this comment
		0.0 == 1);

	compile(status::SUCCESS,
		"type s = struct { a: [2]int }; def x = s { ... }; def y = x.a[1];")!;

	def A: [](int | []str) = [
		0, 0,
		["a"],
		[""],
	];
	assert((A[2] as []str)[0] == "a");

	compile(status::CHECK, "type e = enum { A }; fn f() void = { e::A: bool; };")!;
};
