type foo = void;
type bar = void;
type foobar = (foo | bar);
type baz = int;
type foobarbaz = (foobar | baz);
type signed = (i8 | i16 | i32 | i64 | int);
type unsigned = (u8 | u16 | u32 | u64 | uint | size);
type integer = (...signed | ...unsigned);
type align_4 = (void | int);
type align_8 = (void | int | i64);

fn tagged() void = {
	let cases: [3](int | uint | str) = [10i, 10u, "hello"];
	let expected: [_]size = [1, 2, 5];
	for (let i = 0z; i < len(cases); i += 1) {
		let y: size = match (cases[i]) {
		case int =>
			yield 1;
		case uint =>
			yield 2;
		case let s: str =>
			yield len(s);
		};
		assert(y == expected[i]);
	};
};

fn termination() void = {
	let x: (int | uint | str) = 1337i;
	for (true) {
		let y: int = match (x) {
		case int =>
			yield 42;
		case uint =>
			abort();
		case str =>
			break;
		};
		assert(y == 42);
		x = "hi";
	};
};

fn _default() void = {
	let x: (int | uint | str) = 1337u;
	let y: int = match (x) {
	case int =>
		yield 42;
	case =>
		yield 24;
	};
	assert(y == 24);
};

fn pointer() void = {
	let x = 42;
	let y: nullable *int = &x;
	let z: int = match (y) {
	case let y: *int =>
		yield *y;
	case null =>
		abort();
	};
	assert(z == 42);

	y = null;
	z = match (y) {
	case *int =>
		abort();
	case null =>
		yield 1337;
	};
	assert(z == 1337);
};

fn alias() void = {
	let cases: []foobar = [foo, bar];
	let expected = [42, 24];
	for (let i = 0z; i < len(cases); i += 1) {
		let y: int = match (cases[i]) {
		case foo =>
			yield 42;
		case bar =>
			yield 24;
		};
		assert(y == expected[i]);
	};
};

fn tagged_result() void = {
	let x: (int | uint) = 42i;
	let y: (int | uint) = match (x) {
	case let x: int =>
		yield x;
	case let x: uint =>
		yield x;
	};
	assert(y is int);

	x = 42u;
	y = match (x) {
	case let x: int =>
		yield x;
	case let x: uint =>
		yield x;
	};
	assert(y is uint);
};

fn implicit_cast() void = {
	let x: foobar = foo;
	let y: nullable *int = null;
	let a: (int | foobar) = match (y) {
	case null =>
		yield foo;
	case let z: *int =>
		yield *z;
	};
	assert(a is foobar);
};

fn transitivity() void = {
	let x: (foobar | int) = 10;
	match (x) {
	case let i: int =>
		assert(i == 10);
	case foo =>
		abort();
	case bar =>
		abort();
	};
	x = foo;
	let visit = false;
	match (x) {
	case int =>
		abort();
	case foo =>
		visit = true;
	case bar =>
		abort();
	};
	assert(visit);

	x = bar;
	visit = false;
	match (x) {
	case int =>
		abort();
	case foo =>
		abort();
	case foobar =>
		visit = true;
	};
	assert(visit);

	visit = false;
	match (x) {
	case let z: (foo | bar) =>
		visit = true;
		assert(z is bar);
	case int =>
		abort();
	};
	assert(visit);

	let y: foobarbaz = 10;
	visit = false;
	match (y) {
	case baz =>
		visit = true;
	case foo =>
		abort();
	case bar =>
		abort();
	};
	assert(visit);

	y = foo;
	visit = false;
	match (y) {
	case baz =>
		abort();
	case foo =>
		visit = true;
	case bar =>
		abort();
	};
	assert(visit);
};

fn numeric() void = {
	// Real-world test
	let visit = true;
	let x: integer = 1337i;
	match (x) {
	case let s: signed =>
		match (s) {
		case let i: int =>
			visit = true;
			assert(i == 1337);
		case =>
			abort();
		};
	case let u: unsigned =>
		abort();
	};
	assert(visit);
};

fn alignment_conversion() void = {
	let x: align_8 = 1234i;
	match (x) {
	case let y: align_4 =>
		assert(y as int == 1234);
	case =>
		abort();
	};
	let y: align_4 = 4321i;
	x = y: align_8;
	assert(x as int == 4321);
};

export fn main() void = {
	tagged();
	termination();
	_default();
	pointer();
	alias();
	tagged_result();
	implicit_cast();
	transitivity();
	numeric();
	alignment_conversion();
	// TODO: Test exhaustiveness and dupe detection
};
