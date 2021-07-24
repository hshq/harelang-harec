type coords = struct { x: size, y: size };

fn not(x: bool) bool = x == false;

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

	let q = coords { x = 2, y = 2 };
	let o = &q;
	assert(x[q.x] == 3);
	assert(x[o.x] == 3);

	let f = &not;
	let g = &f;
	assert(not(true) == false);
	assert(f(true) == false);
	assert(g(true) == false);
	return 0;
};
