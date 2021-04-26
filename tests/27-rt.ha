// tests for the test runtime itself

use rt;

fn compile() void = {
	assert(rt::compile("
		fn test() void = {
			void;
		};"
	) == 0);
	assert(rt::compile("
		fn test() void = {
			let a: int = [1, 2, 3];
		};"
	) != 0);
};

export fn main() void = {
	compile();
};
