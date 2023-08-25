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

	defer {
		yield;
	};
	defer {
		for (true) break;
		for (false) continue;
	};
};

fn reject() void = {
	let failures = [
		"export fn main() void = defer 0;",
		"export fn main() void = { if (true) defer 0; };",
		"export fn main() void = { for (defer 0; true; true) void; };",
		"export fn main() void = { defer yield; };",
		"export fn main() void = :outer { defer { yield :outer; }; };",
		"export fn main() void = for (true) { defer break; };",
		"export fn main() void = for (true) { defer continue; };",
		"export fn main() void = { defer defer 0; };",
		"export fn main() void = { defer return; };",
	];

	for (let i = 0z; i < len(failures); i += 1) {
		assert(compile(failures[i]) as exited != EXIT_SUCCESS);
	};
};

fn noreturn() void = {
	defer {
		defer { abort(); };
		defer exit();
	};

	defer x = 30;
	for (true) {
		defer x = 20;
		// above defers are run before this executes, so this is never
		// actually reached
		abort();
	};
};

@noreturn fn exit() void = {
	assert(x == 30);
	x = 0;
	defer rt::exit(x);
	(void: !void: (void | !void))!; // void
	x = 1;
	abort();
};

fn nested() void = {
	let sl: []size = [];
	defer free(sl);
	defer {
		assert(len(sl) == 7);
		for (let i = 0z; i < len(sl); i += 1) {
			assert(sl[i] == i);
		};
	};

	defer {
		defer append(sl, 6);
		defer if (true) {
			append(sl, 3);
			defer append(sl, 5);
			append(sl, 4);
		};
		append(sl, 2);
	};
	defer append(sl, 1);
	append(sl, 0);
};

fn spam() void = {
	// regression test: ensure harec doesn't generate exponential IR here
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
	defer spamfunc()!;
};

fn spamfunc() (void | !void) = void;

export fn main() void = {
	basics();
	assert(x == 20);
	scope();
	loops();
	control();
	reject();
	nested();
	spam();
	noreturn();
};
