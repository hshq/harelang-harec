use rt;
use testmod;

fn accept() void = {
	assert(rt::compile("
		use testmod;
		export fn main() void = static assert(testmod::val == 42);
	") == rt::EXIT_SUCCESS);
	assert(rt::compile("
		use testmod;
		use alias = testmod;
		export fn main() void = static assert(testmod::val == alias::val);
	") == rt::EXIT_SUCCESS);
	assert(rt::compile("
		use testmod;
		use testmod::{val, val2};
		export fn main() void = static assert(
			testmod::val == val && testmod::val2 == val2
		);
	") == rt::EXIT_SUCCESS);
	assert(rt::compile("
		use testmod;
		use testmod::*;
		export fn main() void = static assert(
			testmod::val == val && testmod::val2 == val2
		);
	") == rt::EXIT_SUCCESS);
	assert(rt::compile("
		use testmod;
		use testmod::{alias = val, val2};
		export fn main() void = static assert(
			testmod::val == alias && testmod::val2 == val2
		);
	") == rt::EXIT_SUCCESS);
	assert(rt::compile("
		use testmod;
		use modalias = testmod::{valalias = val, val2};
		export fn main() void = static assert(
			testmod::val == modalias::valalias && testmod::val2 == modalias::val2
		);
	") == rt::EXIT_SUCCESS);
};

fn reject() void = {
	assert(rt::compile("
		use wrong;
		export fn main() void = { testmod::val };
	") == rt::EXIT_FAILURE);
	assert(rt::compile("
		use testmod::{val};
		export fn main() void = static assert(
			testmod::val == 42
		);
	") == rt::EXIT_FAILURE);
	assert(rt::compile("
		use testmod::{val};
		export fn main() void = static assert(
			val2 == 90
		);
	") == rt::EXIT_FAILURE);
	assert(rt::compile("
		use testmod;
		use test = testmod::*;
		export fn main() void = void;
	") == rt::EXIT_FAILURE);
	assert(rt::compile("
		use testmod;
		use testmod*;
		export fn main() void = void;
	") == rt::EXIT_FAILURE);
	assert(rt::compile("
		use testmod::{alias = val, val2};
		export fn main() void = static assert(val == 42);
	") == rt::EXIT_FAILURE);
	assert(rt::compile("
		use modalias = testmod::{valalias = val, val2};
		export fn main() void = static assert(valalias == 42);
	") == rt::EXIT_FAILURE);
};


export fn main() void = {
	accept();
	reject();
};
