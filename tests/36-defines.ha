use rt;

fn import() void = {
	assert(rt::compile("
		use a = rt;
		def b = a::TESTDEFINE;
	", "TESTDEFINE=1": rt::define) as rt::exited != rt::EXIT_SUCCESS);
};

fn mandatory() void = {
	assert(rt::compile("
		def TESTDEFINE = TESTDEFINE;
	", "TESTDEFINE=1": rt::define) as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = TESTDEFINE;
	") as rt::exited != rt::EXIT_SUCCESS);
};

fn optional() void = {
	assert(rt::compile("
		def TESTDEFINE = 1;
		fn main() void = {
			static assert(TESTDEFINE == 2);
		};
	", "TESTDEFINE=2": rt::define) as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 1;
		fn main() void =  {
			static assert(TESTDEFINE == 1);
		};
	") as rt::exited == rt::EXIT_SUCCESS);
};

fn compatibility() void = {
	assert(rt::compile("
		def TESTDEFINE: int = 0;
	", "TESTDEFINE:int=1": rt::define) as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE: int = 0;
	", "TESTDEFINE:uint=1": rt::define) as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		const TEST = 1;
	", "TEST=0": rt::define) as rt::exited != rt::EXIT_SUCCESS);
};

export fn main() void = {
	import();
	mandatory();
	optional();
	// TODO: compatibility();
};
