// tests for the test runtime itself

use rt;

fn compile() void = {
	assert(rt::compile("
		fn test() void = {
			void;
		};"
	) as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		fn test() void = {
			let a: int = [1, 2, 3];
		};"
	) as rt::exited != rt::EXIT_SUCCESS);
};

export fn main() void = {
	compile();
};
