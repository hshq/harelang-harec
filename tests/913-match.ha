fn subtype() void = {
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

type foo = (int | void);
type bar = (size | foo);

fn nested_subtype() void = {
	let x: bar = 1337;
	match (x) {
		i: int => assert(i == 1337),
		size => abort(),
		void => abort(),
	};
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

export fn main() int = {
	subtype();
	nested_subtype();
	pointer();
	return 0;
};
