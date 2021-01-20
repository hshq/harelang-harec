let x: int = 10;

fn basics() void = {
	assert(x == 10);
	defer x = 20;
	assert(x == 10);
};

fn scope() void = {
	let x = 10;
	{
		defer x = 20;
		assert(x == 10);
	};
	assert(x == 20);
};

fn loops() void = {
	let x = 0;
	for (let i = 0; i < 5; i += 1) {
		defer x += 1;
		assert(x == i);
	};
	assert(x == 5);
};

fn control() void = {
	let x = 0;
	for (let i = 0; i < 5; i += 1) {
		if (true) {
			continue;
		};
		defer x += 1;
	};
	assert(x == 0);

	for (let i = 0; i < 5; i += 1) {
		defer x += 1;
		if (true) {
			break;
		};
	};
	assert(x == 1);
};

export fn main() void = {
	basics();
	assert(x == 20);
	scope();
	loops();
	control();
};
