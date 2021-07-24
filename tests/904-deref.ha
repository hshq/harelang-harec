type coords = struct { x: int, y: int };

export fn main() int = {
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
	return 0;
};
