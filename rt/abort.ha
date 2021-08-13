export @noreturn @symbol("rt.abort") fn _abort(msg: str) void = {
	const prefix = "Abort: ";
	write(2, constchar(prefix), len(prefix));
	write(2, constchar(msg), len(msg));
	write(2, constchar("\n"), 1);
	kill(getpid(), SIGABRT);
};

// See harec:include/gen.h
const reasons: [_]str = [
	"slice or array access out of bounds",	// 0
	"type assertion failed",		// 1
	"out of memory",			// 2
	"unreachable code",			// 3
];

export @noreturn fn abort_fixed(loc: str, i: int) void = {
	const prefix = "Abort: ";
	const sep = ": ";
	write(2, constchar(prefix), len(prefix));
	write(2, constchar(loc), len(loc));
	write(2, constchar(sep), len(sep));
	write(2, constchar(reasons[i]), len(reasons[i]));
	write(2, constchar("\n"), 1);
	kill(getpid(), SIGABRT);
};
