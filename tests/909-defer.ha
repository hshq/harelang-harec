let x: int = 0;

fn basics() void = {
	x = 10;
	defer x += 1;
	assert(x == 10);
	defer x += 1;
	x = 20;
};

export fn main() int = {
	basics();
	assert(x == 22);
	return 0;
};
