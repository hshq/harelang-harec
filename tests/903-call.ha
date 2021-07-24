type coords = struct { x: int, y: int };

fn foo() size = 2;
fn equal(x: int, y: int) bool = x == y;
fn aggregate(c: coords) coords = c;

export fn main() int = {
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
	return 0;
};
