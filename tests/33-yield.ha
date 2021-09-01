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

fn termination() void = {
	:outer {
		if (true) {
			yield :outer;
		} else void;
		abort();
	};
};

fn cast_lowering() void = {
	let x: (int | void) = {
		yield 10;
	};
	assert(x as int == 10);
};

export fn main() void = {
	basics();
	termination();
	cast_lowering();
};
