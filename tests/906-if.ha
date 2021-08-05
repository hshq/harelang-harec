type coords = struct { x: int, y: int };

export fn main() int = {
	let x = 10;
	assert((if (false) x else 20) == 20);

	let x = if (true) 10 else abort();
	assert(x == 10);

	let x = if (true) coords {
		x = 10,
		y = 20,
	} else abort();
	assert(x.x == 10);

	if (true) void else void;

	return 0;
};
