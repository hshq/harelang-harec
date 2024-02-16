use rt::{compile, status};

fn scope() void = {
	let x = 0;
	for (let i = 1; i == 1; i += 1) {
		for (true) {
			assert(x == 0);
			assert(i == 1);
			break;
		};
	};
	compile(status::CHECK, "fn test() void = { for (true) { let x = 10; }; x; };")!;
	// To make sure that the afterthought is part of the loop's scope
	for (let i = 0; true; (if (true) { break; })) true;
};

fn conditional() void = {
	let i = 1;
	for (i < 10) {
		i *= 2;
	};
	assert(i == 16);
};

fn afterthought() void = {
	let i = 1;
	for (i < 5; i += 1) {
		i *= 2;
	};
	assert(i == 7);
};

fn binding() void = {
	let x = 0;
	for (let i = 0; i < 10; i += 1) {
		i *= 2;
		x += 1;
	};
	assert(x == 4);
};

fn _break() void = {
	let x = 0;
	for (let i = 0; i < 1; i += 1) {
		let j = 0;
		for (j < 10) {
			j += 1;
			if (j == 5) {
				break;
			};
		};
		assert(j == 5);
		x += 1;
	};
	assert(x == 1);
};

fn _continue() void = {
	let done = false;
	let x = 0;
	for (!done) {
		for (let i = 0; i < 10; i += 1) {
			if (i == 5) {
				continue;
			};
			assert(i != 5);
		};
		done = true;
		x += 1;
	};
	assert(x == 1);
};

fn label() void = {
	let i = 0;
	for :outer (i < 10) {
		for :inner (let j = 0; j < 7; j += 1) {
			i += 1;
			if (j == 6) {
				for (let k = 0; k < 5; k += 1) {
					if (k == 2) {
						continue :inner;
					};
					assert(k < 2);
				};
			};
			assert(j != 6);
			if (i > 7) {
				break :outer;
			};
		};
	};
	assert(i == 8);
	compile(status::CHECK, "fn test() void = { for :foo (true) { break :bar; }; };")!;
	compile(status::CHECK, "fn test() void = { for (true) { break :bar; }; };")!;
	compile(status::CHECK, "fn test() void = { break :bar; };")!;
	compile(status::CHECK, "fn test() void = :foo { break :foo; };")!;
	compile(status::CHECK, "fn test() void = { for :foo (true) { yield :foo; }; };")!;
	compile(status::CHECK, "fn test() void = :foo { for :foo (true) { yield :foo; }; };")!;
	compile(status::CHECK, "fn test() void = for :foo (true) :foo { break :foo; };")!;
};

type abool = bool;

fn alias() void = {
	for (true: abool) {
		return;
	};
};

fn _static() void = {
	let count = 0z;
	for (let i = 0z; i < 2; i += 1) {
		for (static let j = 0z; j < 5; j += 1) {
			count += 1;
		};
	};
	assert(count == 5);
};

fn result() void = {
	for (true) break;
	for :loop (true) {
		for (true) break :loop;
	};
	let integer = switch (0) {
	case 0 => yield 0;
	case => for (true) void;
	};
	assert(integer == 0);
};

export fn main() void = {
	scope();
	conditional();
	afterthought();
	binding();
	_break();
	_continue();
	label();
	alias();
	_static();
	result();
};
