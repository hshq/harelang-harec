use rt::{compile, status};
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
	compile(status::USER, "
		use wrong;
		export fn main() void = { testmod::val; };
	")!;
	compile(status::USER, "
		use testmod::{val};
		export fn main() void = static assert(
			testmod::val == 42
		);
	")!;
	compile(status::USER, "
		use testmod::{val};
		export fn main() void = static assert(
			val2 == 90
		);
	")!;
	compile(status::PARSE, "
		use testmod;
		use test = testmod::*;
		export fn main() void = void;
	")!;
	compile(status::PARSE, "
		use testmod*;
		export fn main() void = void;
	")!;
	compile(status::PARSE, "
		use testmod{val};
		export fn main() void = void;
	")!;
	compile(status::PARSE, "
		use a::b = testmod;
		export fn main() void = void;
	")!;
	compile(status::PARSE, "
		use test:: = testmod;
		export fn main() void = void;
	")!;
	compile(status::PARSE, "
		use testmod::;
		export fn main() void = void;
	")!;
	compile(status::PARSE, "
		use test = testmod::;
		export fn main() void = void;
	")!;
	compile(status::USER, "
		use testmod;
		export fn main() void = { testmod::testmod::val3; };
	")!;
};


export fn main() void = {
	reject();
};
