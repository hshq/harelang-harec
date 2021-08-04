export fn main() int = {
	let x = 0;
	x += 1;
	assert(x == 1);
	let y = &x;
	*y += 1;
	assert(x == 2);
	return 0;
};
