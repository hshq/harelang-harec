use rt::{compile, exited, EXIT_SUCCESS};

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
	assert(compile("
		type error = !void;

		export fn main() int = {
			error;
			return 42;
		};
	") as exited != EXIT_SUCCESS);
	err_if_false(true)!;
};

fn void_assignability() void = {
	assert(compile(`
		type err = !void;

		fn reterr() (int | err) = {
			return err;
		};

		fn properr() void = {
			reterr()?;
		};

		export fn main() void = void;
	`) as exited != EXIT_SUCCESS); // error types cannot be assigned to void

	assert(compile(`
		fn disallow_1() void = {
			return "I am illegal";
		};

		fn disallow_2() void = {
			return 12;
		};

		export fn main() void = void;
	`) as exited != EXIT_SUCCESS); // non-void types cannot be assigned to void
};

export fn main() void = {
	assignability();
	propagate();
	cannotignore();
	void_assignability();
};
