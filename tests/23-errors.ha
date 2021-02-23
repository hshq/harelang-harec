type err_int = int!;

fn assignability() void = {
	// Error and non-error types are interchangable:
	let a: int! = 10;
	let b: int = a;
	assert(a == b);
};

type error = void!;

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

export fn main() void = {
	assignability();
	propagate();
};
