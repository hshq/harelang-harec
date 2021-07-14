export fn main() int = {
	// Direct
	let x: int = 10;
	x = 2 + 2;
	assert(x == 4);

	// Indirect
	let x = [1, 2, 3];
	assert(x[1 + 1] == 3);
	return 0;
};
