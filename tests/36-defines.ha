use rt::{compile, status};

fn import() void = {
	compile(status::CHECK, "
		use a = rt;
		def b = a::TESTDEFINE;
	", "-DTESTDEFINE=1")!;
};

fn mandatory() void = {
	compile(status::SUCCESS, "
		def TESTDEFINE = TESTDEFINE;
	", "-DTESTDEFINE=1")!;
	compile(status::CHECK, "
		def TESTDEFINE = TESTDEFINE;
	")!;
};

fn optional() void = {
	compile(status::SUCCESS, "
		def TESTDEFINE = 1;
		static assert(TESTDEFINE == 2);
	", "-DTESTDEFINE=2")!;
	compile(status::SUCCESS, "
		def TESTDEFINE = 1;
		static assert(TESTDEFINE == 1);
	")!;
};

fn compatibility() void = {
	compile(status::SUCCESS, "
		def TESTDEFINE: int = 0;
	", "-DTESTDEFINE:int=1")!;
	compile(status::CHECK, "
		def TESTDEFINE: int = 0;
	", "-DTESTDEFINE:uint=1")!;
	compile(status::CHECK, "
		const TEST = 1;
	", "-DTEST=0")!;
	compile(status::SUCCESS, "
		def TESTDEFINE = 0;
		static assert(TESTDEFINE == 1);
		let x = TESTDEFINE;
		let y: *u8 = &x;
	", "-DTESTDEFINE:u8=1")!;
	compile(status::SUCCESS, "
		def TESTDEFINE: u8 = 0;
		static assert(TESTDEFINE == 1);
		let x = TESTDEFINE;
		let y: *u8 = &x;
	", "-DTESTDEFINE=1")!;
	compile(status::SUCCESS, "
		def TESTDEFINE = 0u32;
		static assert(TESTDEFINE == 'a');
		let x = TESTDEFINE;
		let y: *u32 = &x;
	", "-DTESTDEFINE='a'")!;
	compile(status::SUCCESS, "
		def TESTDEFINE = 'a';
		static assert(TESTDEFINE == 0);
		let x = TESTDEFINE;
		let y: *u32 = &x;
	", "-DTESTDEFINE=0u32")!;
	compile(status::CHECK, "
		def TESTDEFINE = 0;
	", "-DTESTDEFINE='a'")!;
	compile(status::CHECK, "
		def TESTDEFINE = 0.0;
	", "-DTESTDEFINE=0")!;
	compile(status::SUCCESS, "
		def TESTDEFINE = 0.0;
		static assert(TESTDEFINE == 1.0);
		let x = TESTDEFINE;
		let y: *f32 = &x;
	", "-DTESTDEFINE=1f32")!;
	compile(status::SUCCESS, "
		def TESTDEFINE = 0f32;
		static assert(TESTDEFINE == 1.0);
		let x = TESTDEFINE;
		let y: *f32 = &x;
	", "-DTESTDEFINE=1.0")!;
	compile(status::CHECK, "
		def TESTDEFINE = 255;
		let x: u8 = TESTDEFINE;
	", "-DTESTDEFINE=256")!;
	compile(status::CHECK, "
		def TESTDEFINE = 256;
		let x: u8 = TESTDEFINE;
	", "-DTESTDEFINE=255")!;
};

export fn main() void = {
	import();
	mandatory();
	optional();
	compatibility();
};
