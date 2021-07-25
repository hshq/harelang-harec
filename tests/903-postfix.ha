type coords = struct { x: size, y: size };

fn foo() size = 2;
fn equal(x: int, y: int) bool = x == y;
fn aggregate(c: coords) coords = c;
fn not(x: bool) bool = x == false;

fn deref() void = {
	let a = coords { x = 10, y = 20 };
	let b = &a;
	let c = &b;
	assert(a.x == 10);
	assert(b.x == 10);
	assert(c.x == 10);

	let x = [1, 3, 3, 7];
	let y = &x;
	let z = &y;
	assert(x[2] == 3);
	assert(y[2] == 3);
	assert(z[2] == 3);

	let q = coords { x = 2, y = 2 };
	let o = &q;
	assert(x[q.x] == 3);
	assert(x[o.x] == 3);

	let f = &not;
	let g = &f;
	assert(not(true) == false);
	assert(f(true) == false);
	assert(g(true) == false);
};

fn calls() void = {
	// Indirect
	let x: size = foo();
	assert(x == 2);

	// Direct
	let x = [1, 2, 3];
	assert(x[foo()] == 3);

	// Direct & indirect params
	let x = 1234;
	assert(equal(x, 1234));

	// Aggregate params and return
	let x = coords { x = 1234, y = 4321 };
	let x = aggregate(x);
	// TODO: Use &&
	assert(x.x == 1234);
	assert(x.y == 4321);
};

export fn main() int = {
	deref();
	calls();
	return 0;
};
