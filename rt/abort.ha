export @noreturn @symbol("rt.abort") fn _abort(msg: str) void = {
	const prefix = "Abort: ";
	// XXX: This causes const to fall off, we should catch that (may need
	// spec update)
	write(2, prefix: *const char, len(prefix));
	write(2, msg: *const char, len(msg));
	write(2, "\n": *const char, 1z);
	kill(getpid(), SIGABRT);
};
