use rt::{compile, exited, EXIT_SUCCESS};
use testmod;
use alias = testmod;
use testmod::{val, val2};
use testmod::*;
use testmod::{alias2 = val2};
use modalias = testmod::{valalias = val, val2};

static assert(testmod::val == 42);
static assert(testmod::val == alias::val);
static assert(testmod::val == val);
static assert(testmod::val2 == val2);
static assert(testmod::val2 == alias2);
static assert(testmod::val == modalias::valalias);
static assert(testmod::val2 == modalias::val2);
static assert(testmod::other::EIGHT == 8);

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
	assert(compile("
		use testmod::{a = b = val};
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
};


export fn main() void = {
	reject();
};
