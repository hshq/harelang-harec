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

export fn main() void = {
	tagged();
	termination();
	default();
	pointer();
};
