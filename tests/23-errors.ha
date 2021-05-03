use rt;

type err_int = !int;

fn assignability() void = {
	// Error and non-error types are interchangable:
	let a: !int = 10;
	let b: int = a;
	assert(a == b);
};

type error = !void;

fn err_if_false(in: bool) (error | int) = {
	if (in) {
		return 1337;
	};
	return error;
};

fn indirect(in: bool) (error | int) = {
	let x = err_if_false(in)?;
	return x;
};

fn propagate() void = {
	assert(indirect(true) as int == 1337);
	assert(indirect(false) is error);
};

fn cannotignore() void = {
	assert(rt::compile("type error = void!;

export fn main() int = {
	error;
	return 42;
};") != 0);
	err_if_false(true)!;
};

export fn main() void = {
	assignability();
	propagate();
	cannotignore();
};
