export @noreturn @symbol("rt.abort") fn _abort(msg: str) void = {
	// XXX: these implicit casts call const to fall off because const types
	// are assignable from non-const types. This should not transit pointer
	// secondaries. Write should be updated to *const void, but not before
	// the compiler catches the error.
	const prefix = "Abort: ";
	write(2, prefix: *const char, len(prefix));
	write(2, msg: *const char, len(msg));
	write(2, "\n": *const char, 1z);
	kill(getpid(), SIGABRT);
};
