fn tagged() void = {
	let cases: [3](int | uint | str) = [10i, 10u, "hello"];
	let expected: [_]size = [1, 2, 5];
	for (let i = 0z; i < len(cases); i += 1) {
		let y: size = match (cases[i]) {
			int    => 1,
			uint   => 2,
			s: str => len(s),
		};
		assert(y == expected[i]);
	};
};

fn termination() void = {
	let x: (int | uint | str) = 1337i;
	for (true) {
		let y: int = match (x) {
			int  => 42,
			uint => abort(),
			str  => break,
		};
		assert(y == 42);
		x = "hi";
	};
};

fn default() void = {
	let x: (int | uint | str) = 1337u;
	let y: int = match (x) {
		int => 42,
		*   => 24,
	};
	assert(y == 24);
};

fn pointer() void = {
	let x = 42;
	let y: nullable *int = &x;
	let z: int = match (y) {
		y: *int => *y,
		null    => abort(),
	};
	assert(z == 42);
	
	y = null;
	z = match(y) {
		*int => abort(),
		null => 1337,
	};
	assert(z == 1337);
};

type foo = void;
type bar = void;
type foobar = (foo | bar);

fn alias() void = {
	let cases: []foobar = [foo, bar];
	let expected = [42, 24];
	for (let i = 0z; i < len(cases); i += 1) {
		let y: int = match (cases[i]) {
			foo => 42,
			bar => 24,
		};
		assert(y == expected[i]);
	};
};

fn tagged_result() void = {
	let x: (int | uint) = 42i;
	let y: (int | uint) = match (x) {
		x: int  => x,
		x: uint => x,
	};
	assert(y is int);

	x = 42u;
	y = match (x) {
		x: int  => x,
		x: uint => x,
	};
	assert(y is uint);
};

fn implicit_cast() void = {
	let x: foobar = foo;
	let y: nullable *int = null;
	let a: (int | foobar) = match (y) {
		null => foo,
		z: *int => *z,
	};
	assert(a is foobar);
};

type foo = void;
type bar = void;
type foobar = (foo | bar);
type baz = int;
type foobarbaz = (foobar | baz);

fn transitivity() void = {
	let x: (foobar | int) = 10;
	match (x) {
		i: int => assert(i == 10),
		foo => abort(),
		bar => abort(),
	};
	x = foo;
	let visit = false;
	match (x) {
		int => abort(),
		foo => {
			visit = true;
		},
		bar => abort(),
	};
	assert(visit);

	x = bar;
	visit = false;
	match (x) {
		int    => abort(),
		foo    => abort(),
		foobar => {
			visit = true;
		},
	};
	assert(visit);

	visit = false;
	match (x) {
		z: (foo | bar) => {
			visit = true;
			assert(z is bar);
		},
		int => abort(),
	};
	assert(visit);

	let y: foobarbaz = 10;
	visit = false;
	match (y) {
		baz => {
			visit = true;
		},
		foo => abort(),
		bar => abort(),
	};
	assert(visit);

	y = foo;
	visit = false;
	match (y) {
		baz => abort(),
		foo => {
			visit = true;
		},
		bar => abort(),
	};
	assert(visit);
};

type signed = (i8 | i16 | i32 | i64 | int);
type unsigned = (u8 | u16 | u32 | u64 | uint | size);
type integer = (...signed | ...unsigned);

export fn numeric() void = {
	// Real-world test
	let visit = true;
	let x: integer = 1337i;
	match (x) {
		s: signed => match (s) {
			i: int => {
				visit = true;
				assert(i == 1337);
			},
			* => abort(),
		},
		u: unsigned => abort(),
	};
	assert(visit);
};

type align_4 = (void | int);
type align_8 = (void | int | i64);

export fn alignment_conversion() void = {
	let x: align_8 = 1234i;
	match (x) {
		y: align_4 => assert(y as int == 1234),
		* => abort(),
	};
	let y: align_4 = 4321i;
	x = y: align_8;
	assert(x as int == 4321);
};

export fn main() void = {
	tagged();
	termination();
	default();
	pointer();
	alias();
	tagged_result();
	implicit_cast();
	transitivity();
	numeric();
	alignment_conversion();
	// TODO: Test exhaustiveness and dupe detection
};
