fn basics() void = {
	let cases = [[0, 1], [1, 3], [10, 20], [11, 21], [12, 22], [13, 13]];
	for (let i = 0z; i < len(cases); i += 1) {
		let x = cases[i][0];
		let y: int = switch (x) {
		case 0 =>
			yield x + 1;
		case 1 =>
			yield x + 2;
		case 10, 11, 12 =>
			yield x + 10;
		case =>
			yield x;
		};
		assert(y == cases[i][1]);
	};
};

fn _never() void = {
	let x = 42;
	let y: int = switch (x) {
	case 42 =>
		yield 1337;
	case 24 =>
		abort();
	case =>
		abort();
	};
	assert(y == 1337);
};

fn tagged_result() void = {
	let x = 42;
	let y: (int | uint) = switch (x) {
	case 42 =>
		yield 1337i;
	case =>
		yield 1337u;
	};
	assert(y is int);

	x = 24;
	y = switch (x) {
	case 42 =>
		yield 1337i;
	case =>
		yield 1337u;
	};
	assert(y is uint);
};

fn str_switching() void = {
	let result = switch ("hare") {
	case "world" =>
		abort();
	case "hare" =>
		yield true;
	case =>
		abort();
	};
	assert(result == true);
};

fn binding() void = {
	switch (1) {
	case =>
		let x = 42;
	};
};

export fn main() void = {
	basics();
	_never();
	tagged_result();
	str_switching();
	binding();
	// TODO: Test exhaustiveness and dupe detection
};
