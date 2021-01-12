fn rt::compile(src: str) int;

let x: int = 42;

fn write() void = {
	assert(x == 42);
	x = 1337;
	assert(x == 1337);
};

fn invariants() void = {
	assert(rt::compile("fn test() int; let x: int = test();") != 0);
};

export fn main() void = {
	// TODO: Expand this test:
	// - Declare & validate globals of more types
	// - Globals which are pointers to other globals
	write();
	invariants();
};
