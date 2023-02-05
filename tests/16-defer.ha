use rt;
use rt::{compile, exited, EXIT_SUCCESS};

let x: int = 10;

fn basics() void = {
	assert(x == 10);
	defer x = 20;
	assert(x == 10);
	if (true) {
		return;
	};
	defer x = 30;
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
		abort();
	};
	assert(x == 1);
};

fn reject() void = {
	let failures = [
		"export fn main() void = defer 0;",
		"export fn main() void = { if (true) defer 0; };",
		"export fn main() void = { for (defer 0; true; true) void; };",
	];

	for (let i = 0z; i < len(failures); i += 1) {
		assert(compile(failures[i]) as exited != EXIT_SUCCESS);
	};
};

fn noreturn() void = {
	defer x = 30;
	for (true) {
		defer x = 20;
		exit();
	};
};

@noreturn fn exit() void = {
	assert(x == 30);
	rt::exit(0);
};

export fn main() void = {
	basics();
	assert(x == 20);
	scope();
	loops();
	control();
	reject();
	noreturn();
};
