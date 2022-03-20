use rt;

fn simple() int = return 69;

fn addone(x: *int) void = {
	*x += 1;
};

fn pointers() void = {
	let x = 0;
	addone(&x);
	assert(x == 1);
	let y = &addone;
	y(&x);
	assert(x == 2);
	let z = &y;
	z(&x);
	assert(x == 3);
};

fn vafn(expected: []int, values: int...) void = {
	assert(len(expected) == len(values));
	for (let i = 0z; i < len(values); i += 1) {
		assert(expected[i] == values[i]);
	};
};

fn vaargs() void = {
	vafn([1, 2, 3], 1, 2, 3);
	let data = [1, 2, 3];
	vafn(data, data...);
	vafn([]);
};

let x: int = 42;

@init fn init() void = {
	x = 1337;
};

@init fn init() void = {
	void; // Should be allowed to have the same name
};

@fini fn fini() void = {
	rt::exit(42); // Magic number
};

@fini fn fini() void = {
	void; // Should be allowed to have the same name
};

fn cvafn(n: int, ...) void = {
	let ap = vastart();
	defer vaend(ap);
	let ap2 = ap;
	defer vaend(ap2);
	for (let i = 0; i < n; i += 1) {
		let arg: int = vaarg(ap);
		assert(arg == i + 1);
	};
	for (let i = 0; i < n; i += 1) {
		let arg: int = vaarg(ap2);
		assert(arg == i + 1);
	};
};

fn cvaargs() void = {
	cvafn(3, 1, 2, 3);
};

export fn main() void = {
	assert(simple() == 69);
	pointers();
	vaargs();
	cvaargs();
	assert(x == 1337);
};
