use rt::{compile, status};

fn basics() void = {
	let x = {
		yield 10;
	};
	let y = :outer {
		if (true) {
			yield :outer, 20;
		};
		abort();
	};
	assert(x == 10);
	assert(y == 20);
};

fn _never() void = {
	:outer {
		let x: int = if (true) {
			yield :outer;
		} else 1;
		abort();
	};

	compile(status::CHECK,
		"fn test() void = { let x: int = if (true) { yield; } else 1; };"
	)!;
};

fn cast_lowering() void = {
	let x: (int | void) = {
		yield 10;
	};
	assert(x as int == 10);

	let x = {
		for (false) yield 10;
	};
	assert(x is void);
};

fn shadowing() void = {
	let i = 0;
	:foo {
		:foo {
			for :foo (true) {
				break :foo;
			};
			i += 1;
			yield :foo;
		};
		i += 1;
		yield :foo;
	};
	assert(i == 2);
};

export fn main() void = {
	basics();
	_never();
	cast_lowering();
	shadowing();
};
