use rt::{compile, status, toutf8};

type t = int;
type e = !int;

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

	// assignability
	switch (0) {
	case 0i8 => void;
	case 1i16 => abort();
	case 2: !int => abort();
	case 3: t => abort();
	case 4: !t => abort();
	case => abort();
	};

	// regression test
	const x: e = 0;
	switch (x) {
	case 0 => void;
	case 1: e => abort();
	case => abort();
	};

	compile(status::CHECK, "
		fn test() void = switch (0.0) {
		case 0.0 => void;
		case 1.0 => void;
		case => void;
		};
	")!;

	compile(status::CHECK, "
		fn test(x: int) void = switch (0) {
		case x => void;
		case => void;
		};
	")!;

	compile(status::CHECK, "
		fn test() void = switch (&0) {
		case &0 => void;
		case => void;
		};
	")!;

	compile(status::CHECK, "
		fn test() void = switch (0i32) {
		case 0i64 => void;
		case => void;
		};
	")!;
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
	for :outer (let i: u8 = '0'; i <= '2'; i += 1) {
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
	compile(status::SUCCESS, *(&buf: *str))!;

	compile(status::CHECK, "
		fn test() void = switch (0) {
		case 0 => void;
		};
	")!;

	compile(status::CHECK, "
		type x = enum { A, B, C };
		fn test() void = switch (x::A) {
		case x::A => void;
		case x::B => void;
		};
	")!;
};

fn duplicates() void = {
	compile(status::CHECK, "
		fn test() void = switch (0) {
		case 0 => void;
		case 1 => void;
		case 2, 0, 3 => void;
		case => void;
		};
	")!;

	compile(status::CHECK, "
		fn test() void = switch (0) {
		case 0 => void;
		case => void;
		case => void;
		};
	")!;

	compile(status::CHECK, "
		let x = 0;
		fn test() void = switch (&x) {
		case &x => void;
		case &x => void;
		case => void;
		};
	")!;

	compile(status::CHECK, "
		let x = ([1, 2, 3, 4], 5);
		fn test() void = switch (&0) {
		case &(x.0: [*]int)[4] => void;
		case &x.1 => void;
		case => void;
		};
	")!;

	// TODO: uncomment this once translation-time unions actually work
	//compile(status::CHECK, "
	//	type t = union { x: [2]int, y: (int, int) };
	//	let x = t { ... };
	//	fn test() void = switch (&0) {
	//	case &x.x[1] => void;
	//	case &x.y.1 => void;
	//	case => void;
	//	};
	//")!;

	compile(status::CHECK, "
		type x = enum { A, B, C = B };
		fn test() void = switch (x::A) {
		case x::A => void;
		case x::B => void;
		case x::C => void;
		};
	")!;
};

fn label() void = {
	switch :foo (0) {
	case 0 =>
		if (true) { yield :foo; };
		abort();
	case =>
		if (true) abort(); // unreachable
		// but still test that this branch inherits the label
		yield :foo;
	};
};

export fn main() void = {
	basics();
	tagged_result();
	binding();
	exhaustivity();
	duplicates();
	label();
};
