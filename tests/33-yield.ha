use rt::{compile, exited, EXIT_SUCCESS};

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

	assert(compile(
		"fn test() void = { let x: int = if (true) { yield; } else 1; };"
	) as exited != EXIT_SUCCESS);
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

export fn main() void = {
	basics();
	_never();
	cast_lowering();
};
