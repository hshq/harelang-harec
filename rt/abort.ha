export @noreturn @symbol("rt.abort") fn _abort(loc: str, msg: str) void = {
	const prefix = "Abort: ";
	const sep = ": ";
	write(2, constchar(prefix), len(prefix));
	write(2, constchar(loc), len(loc));
	write(2, constchar(sep), len(sep));
	write(2, constchar(msg), len(msg));
	write(2, constchar("\n"), 1);
	kill(getpid(), SIGABRT);
};

// See harec:include/gen.h
const reasons: [_]str = [
	"slice or array access out of bounds",	// 0
	"type assertion failed",		// 1
	"out of memory",			// 2
	"static append exceeds slice capacity",	// 3
	"unreachable code",			// 4
	"slice allocation capacity smaller than initializer",	// 5
	"assertion failed",			// 6
	"error occured",			// 7
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
