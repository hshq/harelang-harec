fn rt::compile(src: str) int;

fn scope() void = {
	let x = 0;
	for (let i = 1; i == 1; i += 1) {
		for (true) {
			assert(x == 0);
			assert(i == 1);
			break;
		};
	};
	assert(rt::compile("fn test() void = { for (true) { let x = 10; }; x; };") != 0);
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
	:outer for (i < 10) {
		:inner for (let j = 0; j < 7; j += 1) {
			i += 1;
			if (j == 6) {
				continue :inner;
			};
			assert(j != 6);
			if (i > 7) {
				break :outer;
			};
		};
	};
	assert(i == 8);
	assert(rt::compile("fn test() void = { :foo for (true) { break :bar; }; };") != 0);
	assert(rt::compile("fn test() void = { for (true) { break :bar; }; };") != 0);
	assert(rt::compile("fn test() void = { break :bar; };") != 0);
	assert(rt::compile("fn test() void = { :foo for (true) { :foo for (true) void; } ; };") != 0);
};

export fn main() void = {
	scope();
	conditional();
	afterthought();
	binding();
	_break();
	_continue();
	label();
};
