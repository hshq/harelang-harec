export @noreturn @symbol("rt.abort") fn _abort(msg: str) void = {
	const prefix = "Abort: ";
	write(2, prefix: *char, len(prefix));
	write(2, msg: *char, len(msg));
	write(2, "\n": *char, 1);
	kill(getpid(), SIGABRT);
};
