use rt;

let x: int = 42;

fn write() void = {
	assert(x == 42);
	x = 1337;
	assert(x == 1337);
};

fn invariants() void = {
	assert(rt::compile("fn test() int; let x: int = test();") != 0);
};

fn counter() int = {
	static let x = 0;
	x += 1;
	return x;
};

fn static_binding() void = {
	assert(counter() == 1);
	assert(counter() == 2);
	assert(counter() == 3);
};

export fn main() void = {
	// TODO: Expand this test:
	// - Declare & validate globals of more types
	// - Globals which are pointers to other globals
	write();
	invariants();
	static_binding();
};
