use rt::{compile, exited, EXIT_SUCCESS};
use testmod;

fn _enum() void = {
	assert(testmod::other::EIGHT == 8);
	assert(testmod::char_enum::ONE == 1);
};

fn accept() void = {
	assert(compile("
		use testmod;
		export fn main() void = static assert(testmod::val == 42);
	") as exited == EXIT_SUCCESS);
	assert(compile("
		use testmod;
		use alias = testmod;
		export fn main() void = static assert(testmod::val == alias::val);
	") as exited == EXIT_SUCCESS);
	assert(compile("
		use testmod;
		use testmod::{val, val2};
		export fn main() void = static assert(
			testmod::val == val && testmod::val2 == val2
		);
	") as exited == EXIT_SUCCESS);
	assert(compile("
		use testmod;
		use testmod::*;
		export fn main() void = static assert(
			testmod::val == val && testmod::val2 == val2
		);
	") as exited == EXIT_SUCCESS);
	assert(compile("
		use testmod;
		use testmod::{alias = val, val2};
		export fn main() void = static assert(
			testmod::val == alias && testmod::val2 == val2
		);
	") as exited == EXIT_SUCCESS);
	assert(compile("
		use testmod;
		use modalias = testmod::{valalias = val, val2};
		export fn main() void = static assert(
			testmod::val == modalias::valalias && testmod::val2 == modalias::val2
		);
	") as exited == EXIT_SUCCESS);
};

fn reject() void = {
	assert(compile("
		use wrong;
		export fn main() void = { testmod::val };
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use testmod::{val};
		export fn main() void = static assert(
			testmod::val == 42
		);
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use testmod::{val};
		export fn main() void = static assert(
			val2 == 90
		);
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use testmod;
		use test = testmod::*;
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use testmod;
		use testmod*;
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use testmod::{alias = val, val2};
		export fn main() void = static assert(val == 42);
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use modalias = testmod::{valalias = val, val2};
		export fn main() void = static assert(valalias == 42);
	") as exited != EXIT_SUCCESS);
};


export fn main() void = {
	_enum();
	accept();
	reject();
};
