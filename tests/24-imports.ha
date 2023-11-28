use rt::{compile, exited, EXIT_SUCCESS};
use testmod;
use alias = testmod;
use testmod::*;

static assert(testmod::val == 42);
static assert(testmod::val == alias::val);
static assert(testmod::val == val);
static assert(testmod::val2 == val2);
static assert(testmod::other::EIGHT == 8);

let x: int = testmod::val;
let y: u8 = testmod::val;

fn reject() void = {
	assert(compile("
		use wrong;
		export fn main() void = { testmod::val; };
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
		use testmod*;
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use testmod{val};
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use a::b = testmod;
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use test:: = testmod;
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use testmod::;
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use test = testmod::;
		export fn main() void = void;
	") as exited != EXIT_SUCCESS);
	assert(compile("
		use testmod;
		export fn main() void = { testmod::testmod::val3; };
	") as exited != EXIT_SUCCESS);
};


export fn main() void = {
	reject();
};
