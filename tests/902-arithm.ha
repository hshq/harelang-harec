export fn main() int = {
	// Indirect
	let x: int = 10;
	x = 2 + 2;
	assert(x == 4);

	// Direct
	let x = [1, 2, 3];
	assert(x[1 + 1] == 3);
	return 0;
};
