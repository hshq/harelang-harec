fn basics() void = {
	let cases = [[0, 1], [1, 3], [10, 20], [11, 21], [12, 22], [13, 13]];
	for (let i = 0z; i < len(cases); i += 1z) {
		let x = cases[i][0];
		let y: int = switch (x) {
			0          => x + 1,
			1          => x + 2,
			10, 11, 12 => x + 10,
			*          => {
				x;
			},
		};
		assert(y == cases[i][1]);
	};
};

fn termination() void = {
	let x = 42;
	let y: int = switch (x) {
		42 => 1337,
		24 => abort(),
		*  => abort(),
	};
	assert(y == 1337);
};

fn tagged_result() void = {
	let x = 42;
	let y: (int | uint) = switch (x) {
		42 => 1337,
		*  => 1337u,
	};
	assert(y is int);

	x = 24;
	y = switch (x) {
		42 => 1337,
		*  => 1337u,
	};
	assert(y is uint);
};

export fn main() void = {
	basics();
	termination();
	tagged_result();
	// TODO: Test exhaustiveness and dupe detection
};
