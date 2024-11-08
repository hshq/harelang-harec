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
type aint = int;
type bint = aint;

fn match_tagged_simple(x: *(int | f64 | void)) int = {
	match (x) {
	case void =>
		return 0;
	case let i: *int =>
		*i = *i + 1;
		return 1;
	case let f: *f64 =>
		*f = *f / 2.0;
		return 2;
	};
};

fn tagged_ptr() void = {
	let x: (int | f64 | void) = void;
	assert(match_tagged_simple(&x) == 0);
	assert(x is void);

	x = 4;
	assert(match_tagged_simple(&x) == 1);
	assert(x as int == 5);

	x = 4.0;
	assert(match_tagged_simple(&x) == 2);
	assert(x as f64 == 2.0);

	x = 4;
	match (&x) {
	case void => abort();
	case let t: *(int | f64) =>
		assert(*t as int == 4);
		let v: (int | f64) = 4.0;
		*t = v;
	};
	assert(x as f64 == 4.0);

	let y: nullable *(int | f64 | void) = null;
	match (y) {
	case null => yield;
	case => abort();
	};

	match (y) {
	case *(int | f64 | void) => abort();
	case => yield;
	};

	match (y) {
	case *int => abort();
	case => yield;
	};

	x = 4;
	y = &x;
	match (y) {
	case let i: *int => *i = *i + 1;
	case => abort();
	};
	assert(x as int == 5);
};

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

fn _never() void = {
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

	y = null;
	z = match (y) {
	case *int =>
		abort();
	case =>
		yield 1337;
	};

	y = null;
	z = match (y) {
	case =>
		abort();
	case null =>
		yield 1337;
	};

	assert(z == 1337);
	let y: nullable *baz = &x;
	let z: int = match (y) {
	case let y: *int =>
		yield *y;
	case null =>
		abort();
	};
	assert(z == 42);
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
	let x: (int | void) = 42i;
	let y: (int | void) = match (x) {
	case let x: int =>
		yield x;
	case let x: void =>
		yield x;
	};
	assert(y is int);

	x = void;
	y = match (x) {
	case let x: int =>
		yield x;
	case let x: void =>
		yield x;
	};
	assert(y is void);

	// XXX: i'm like 90% sure that while implementing
	// https://todo.sr.ht/~sircmpwn/hare/871 this will cause a null pointer
	// dereference
	match (x) {
	case int => abort();
	case => void;
	};
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

	let x: (*int | nullable *int | void) = null;
	// reduces to nullable *int
	let x = match (x) {
	case let x: (*int | nullable *int) =>
		yield x;
	case void => abort();
	};
	assert(x == null);
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
	
	let z: (bint | void) = 10;
	match (z) {
	case aint =>
		void;
	case void =>
		abort();
	};
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

fn binding() void = {
	let x: (int | void) = void;
	match (x) {
	case =>
		let x = 42;
	};
};

fn label() void = {
	match :foo (0: (int | void)) {
	case int =>
		if (true) { yield :foo; };
		abort();
	case =>
		if (true) abort(); // unreachable
		// but still test that this branch inherits the label
		yield :foo;
	};
};

export fn main() void = {
	tagged_ptr();
	tagged();
	_never();
	_default();
	pointer();
	alias();
	tagged_result();
	implicit_cast();
	transitivity();
	numeric();
	alignment_conversion();
	binding();
	label();
	// TODO: Test exhaustiveness and dupe detection
};
