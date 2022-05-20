use rt;
use testmod;

fn accept() void = {
	assert(rt::compile("
		use testmod;
		export fn main() void = { &testmod::testfunc1; };
	") == 0);
	assert(rt::compile("
		use testmod;
		use alias = testmod;
		export fn main() void = static assert(&testmod::testfunc1 == &alias::testfunc1);
	") == 0);
	assert(rt::compile("
		use testmod;
		use testmod::{testfunc1, testfunc2};
		export fn main() void = static assert(
			&testmod::testfunc1 == &testfunc1 && &testmod::testfunc2 == &testfunc2
		);
	") == 0);
	assert(rt::compile("
		use testmod;
		use testmod::*;
		export fn main() void = static assert(
			&testmod::testfunc1 == &testfunc1 && &testmod::testfunc2 == &testfunc2
		);
	") == 0);
	assert(rt::compile("
		use testmod;
		use testmod::{alias = testfunc1, testfunc2};
		export fn main() void = static assert(
			&testmod::testfunc1 == &alias && &testmod::testfunc2 == &testfunc2
		);
	") == 0);
	assert(rt::compile("
		use testmod;
		use modalias = testmod::{fnalias = testfunc1, testfunc2};
		export fn main() void = static assert(
			&testmod::testfunc1 == &modalias::fnalias && &testmod::testfunc2 == &modalias::testfunc2
		);
	") == 0);
};

fn reject() void = {
	assert(rt::compile("
		use wrong;
		export fn main() void = { &testmod::testfunc1 };
	") != 0);
	assert(rt::compile("
		use testmod::{testfunc1};
		export fn main() void = static assert(
			&testmod::testfunc1 != null
		);
	") != 0);
	assert(rt::compile("
		use testmod::{testfunc1};
		export fn main() void = static assert(
			&testfunc2 != null
		);
	") != 0);
	assert(rt::compile("
		use testmod;
		use test = testmod::*;
		export fn main() void = void;
	") != 0);
	assert(rt::compile("
		use testmod;
		use testmod*;
		export fn main() void = void;
	") != 0);
	assert(rt::compile("
		use testmod::{alias = testfunc1, testfunc2};
		export fn main() void = static assert(&testfunc1 != null);
	") != 0);
	assert(rt::compile("
		use modalias = testmod::{fnalias = testfunc1, testfunc2};
		export fn main() void = static assert(&fnalias != null);
	") != 0);
};


export fn main() void = {
	accept();
	reject();
};
