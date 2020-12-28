export fn abort @noreturn void = kill(getpid(), SIGABRT);

export @symbol("sys.assert") fn assert_(cond: bool, msg: str) void = {
	if (!cond) {
		const prefix = "Assertion failed: ";
		write(2, prefix: *char, len(prefix));
		write(2, msg: *char, len(msg));
		write(2, "\n": *char, 1);
		abort();
	};
};

