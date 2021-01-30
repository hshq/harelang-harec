use rt;

let x: int = 42;

fn write() void = {
	assert(x == 42);
	x = 1337;
	assert(x == 1337);
};

let ar: [3]int = [1, 2, 3];
let sl: []int = [1, 2, 3];

fn storage() void = {
	assert(len(ar) == 3z);
	assert(ar[0] == 1 && ar[1] == 2 && ar[2] == 3);

	assert(len(sl) == 3z);
	assert(sl[0] == 1 && sl[1] == 2 && sl[2] == 3);
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
	storage();
	invariants();
	static_binding();
};
