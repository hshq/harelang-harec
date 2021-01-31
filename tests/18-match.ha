fn tagged() void = {
	let cases: [3](int | uint | str) = [10, 10u, "hello"];
	let expected = [1z, 2z, 5z];
	for (let i = 0z; i < len(cases); i += 1z) {
		let y: size = match (cases[i]) {
			int    => 1z,
			uint   => 2z,
			s: str => len(s),
		};
		assert(y == expected[i]);
	};
};

fn termination() void = {
	let x: (int | uint | str) = 1337;
	let y: int = match (x) {
		int  => 42,
		uint => abort(),
		str  => abort(),
	};
	assert(y == 42);
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
	for (let i = 0z; i < len(cases); i += 1z) {
		let y: int = match (cases[i]) {
			foo => 42,
			bar => 24,
		};
		assert(y == expected[i]);
	};
};

fn tagged_result() void = {
	let x: (int | uint) = 42;
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
		null => foo: foobar,
		z: *int => *z,
	};
	assert(a is foobar);
};

export fn main() void = {
	tagged();
	termination();
	default();
	pointer();
	alias();
	tagged_result();
	implicit_cast();
	// TODO: Test exhaustiveness and dupe detection
};
