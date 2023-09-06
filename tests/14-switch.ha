use rt::{compile, exited, EXIT_SUCCESS, toutf8};

let x = 1337;

fn basics() void = {
	let cases = [[0, 1], [1, 3], [10, 20], [11, 21], [12, 22], [13, 13]];
	for (let i = 0z; i < len(cases); i += 1) {
		let x = cases[i][0];
		let y: int = switch (x) {
		case 0 =>
			yield x + 1;
		case 1 =>
			yield x + 2;
		case =>
			yield x;
		case 10, 11, 12 =>
			yield x + 10;
		};
		assert(y == cases[i][1]);
	};

	let result = switch ("hare") {
	case "uhhh java" =>
		abort();
	case "hare" =>
		yield true;
	case =>
		abort();
	};
	assert(result);

	let y = &x;
	switch (y) {
	case &x => void;
	case => abort();
	};
	switch (y: *uint) {
	case &x: *uint => void;
	case => abort();
	};

	assert(compile("
		fn test() void = switch (0.0) {
		case 0.0 => void;
		case 1.0 => void;
		case => void;
		};
	") as exited != EXIT_SUCCESS);

	assert(compile("
		fn test(x: int) void = switch (0) {
		case x => void;
		case => void;
		};
	") as exited != EXIT_SUCCESS);

	assert(compile("
		fn test() void = switch (&0) {
		case &0 => void;
		case => void;
		};
	") as exited != EXIT_SUCCESS);
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

fn binding() void = {
	switch (1) {
	case =>
		let x = 42;
	};
};

type a = enum { A, B, C };
type b = enum { A, B, C = B };

fn exhaustivity() void = {
	switch (true) {
	case true => void;
	case false => abort();
	};

	switch (a::A) {
	case a::A => void;
	case a::B, a::C => abort();
	};
	switch (a::A) {
	case a::B, a::C => abort();
	case => void;
	};

	switch (b::B) {
	case b::A => abort();
	case b::C => void;
	};
	switch (b::C) {
	case b::A => abort();
	case b::B => void;
	};

	let buf: [4096]u8 = [0...];
	let buf = buf[..0];
	static append(buf, toutf8("fn test() void = switch (0u8) {")...);
	for (let i: u8 = '0'; i <= '2'; i += 1) :outer {
		for (let j: u8 = '0'; j <= '9'; j += 1) {
			for (let k: u8 = '0'; k <= '9'; k += 1) {
				if (i == '2' && j == '5' && k == '6') {
					break :outer;
				};
				static append(buf, toutf8("case ")...);
				if (i != '0') {
					static append(buf, i);
				};
				if (i != '0' || j != '0') {
					static append(buf, j);
				};
				static append(buf, k);
				static append(buf, toutf8("=> void;")...);
			};
		};
	};
	static append(buf, toutf8("};")...);
	assert(compile(*(&buf: *str)) as exited == EXIT_SUCCESS);

	assert(compile("
		fn test() void = switch (0) {
		case 0 => void;
		};
	") as exited != EXIT_SUCCESS);

	assert(compile("
		type x = enum { A, B, C };
		fn test() void = switch (x::A) {
		case x::A => void;
		case x::B => void;
		};
	") as exited != EXIT_SUCCESS);
};

fn duplicates() void = {
	assert(compile("
		fn test() void = switch (0) {
		case 0 => void;
		case 1 => void;
		case 2, 0, 3 => void;
		case => void;
		};
	") as exited != EXIT_SUCCESS);

	assert(compile("
		fn test() void = switch (0) {
		case 0 => void;
		case => void;
		case => void;
		};
	") as exited != EXIT_SUCCESS);

	assert(compile("
		let x = 0;
		fn test() void = switch (&x) {
		case &x => void;
		case &x => void;
		case => void;
		};
	") as exited != EXIT_SUCCESS);

	assert(compile("
		let x = ([1, 2, 3, 4], 5);
		fn test() void = switch (&0) {
		case &(x.0: [*]int)[4] => void;
		case &x.1 => void;
		case => void;
		};
	") as exited != EXIT_SUCCESS);

	// TODO: uncomment this once translation-time unions actually work
	//assert(compile("
	//	type t = union { x: [2]int, y: (int, int) };
	//	let x = t { ... };
	//	fn test() void = switch (&0) {
	//	case &x.x[1] => void;
	//	case &x.y.1 => void;
	//	case => void;
	//	};
	//") as exited != EXIT_SUCCESS);

	assert(compile("
		type x = enum { A, B, C = B };
		fn test() void = switch (x::A) {
		case x::A => void;
		case x::B => void;
		case x::C => void;
		};
	") as exited != EXIT_SUCCESS);
};

export fn main() void = {
	basics();
	tagged_result();
	binding();
	exhaustivity();
	duplicates();
};
