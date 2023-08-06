use rt;

fn import() void = {
	assert(rt::compile("
		use a = rt;
		def b = a::TESTDEFINE;
	", "-DTESTDEFINE=1") as rt::exited != rt::EXIT_SUCCESS);
};

fn mandatory() void = {
	assert(rt::compile("
		def TESTDEFINE = TESTDEFINE;
	", "-DTESTDEFINE=1") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = TESTDEFINE;
	") as rt::exited != rt::EXIT_SUCCESS);
};

fn optional() void = {
	assert(rt::compile("
		def TESTDEFINE = 1;
		fn test() void = {
			static assert(TESTDEFINE == 2);
		};
	", "-DTESTDEFINE=2") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 1;
		fn test() void =  {
			static assert(TESTDEFINE == 1);
		};
	") as rt::exited == rt::EXIT_SUCCESS);
};

fn compatibility() void = {
	assert(rt::compile("
		def TESTDEFINE: int = 0;
	", "-DTESTDEFINE:int=1") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE: int = 0;
	", "-DTESTDEFINE:uint=1") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		const TEST = 1;
	", "-DTEST=0") as rt::exited != rt::EXIT_SUCCESS);
};

export fn main() void = {
	import();
	mandatory();
	optional();
	compatibility();
};
