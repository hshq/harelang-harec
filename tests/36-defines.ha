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
		fn main() void = {
			static assert(TESTDEFINE == 2);
		};
	", "-DTESTDEFINE=2") as rt::exited == rt::EXIT_SUCCESS);
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
	", "-DTESTDEFINE:int=1") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE: int = 0;
	", "-DTESTDEFINE:uint=1") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		const TEST = 1;
	", "-DTEST=0") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 0;
		static assert(TESTDEFINE == 1);
		let x = TESTDEFINE;
		let y: *u8 = &x;
	", "-DTESTDEFINE:u8=1") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE: u8 = 0;
		static assert(TESTDEFINE == 1);
		let x = TESTDEFINE;
		let y: *u8 = &x;
	", "-DTESTDEFINE=1") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 0u32;
		static assert(TESTDEFINE == 'a');
		let x = TESTDEFINE;
		let y: *u32 = &x;
	", "-DTESTDEFINE='a'") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 'a';
		static assert(TESTDEFINE == 0);
		let x = TESTDEFINE;
		let y: *u32 = &x;
	", "-DTESTDEFINE=0u32") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 0;
	", "-DTESTDEFINE='a'") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 0.0;
	", "-DTESTDEFINE=0") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 0.0;
		static assert(TESTDEFINE == 1.0);
		let x = TESTDEFINE;
		let y: *f32 = &x;
	", "-DTESTDEFINE=1f32") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 0f32;
		static assert(TESTDEFINE == 1.0);
		let x = TESTDEFINE;
		let y: *f32 = &x;
	", "-DTESTDEFINE=1.0") as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 255;
		let x: u8 = TESTDEFINE;
	", "-DTESTDEFINE=256") as rt::exited != rt::EXIT_SUCCESS);
	assert(rt::compile("
		def TESTDEFINE = 256;
		let x: u8 = TESTDEFINE;
	", "-DTESTDEFINE=255") as rt::exited != rt::EXIT_SUCCESS);
};

export fn main() void = {
	import();
	mandatory();
	optional();
	compatibility();
};
