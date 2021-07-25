type coords = struct { x: size, y: size };
type coords3 = struct { _2: coords, z: size };

fn foo() size = 2;
fn equal(x: int, y: int) bool = x == y;
fn aggregate(c: coords) coords = c;
fn not(x: bool) bool = x == false;

fn nested() void = {
	let c = coords3 {
		_2 = coords {
			x = 10,
			y = 20,
		},
		z = 30,
	};
	assert(c._2.x == 10);
	assert(c._2.y == 20);
	assert(c.z == 30);

	let x = coords { x = 10, y = 20 };
	let a = [x, x, x, x];
	assert(a[0].x == 10);
	assert(a[0].y == 20);
	assert(a[1].x == 10);
	assert(a[1].y == 20);
	assert(a[2].x == 10);
	assert(a[2].y == 20);
	assert(a[3].x == 10);
	assert(a[3].y == 20);
};

fn nonaccess() void = {
	let c = coords { x = 10, y = 20 };
	assert(aggregate(coords { x = 10, y = 20 }).x == 10);
	assert(aggregate(c).y == 20);
	assert(coords { x = 10, y = 20 }.x == 10);
	assert(coords { x = 10, y = 20 }.y == 20);
	assert([1, 2, 3, 4][2] == 3);
};

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
	nested();
	nonaccess();
	deref();
	calls();
	return 0;
};
