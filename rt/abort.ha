export @symbol("rt.abort") fn _abort(
	path: *str,
	line: u64,
	col: u64,
	msg: str,
) void = {
	const prefix = "Abort: ";
	const sep = ":";
	const sepspace = ": ";
	write(2, constchar(prefix), len(prefix));
	write(2, constchar(*path), len(path));
	write(2, constchar(sep), len(sep));
	let (line, z) = u64tos(line);
	write(2, line, z);
	write(2, constchar(sep), len(sep));
	let (col, z) = u64tos(col);
	write(2, col, z);
	write(2, constchar(sepspace), len(sepspace));
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
	"error occurred",			// 7
];

export fn abort_fixed(path: *str, line: u64, col: u64, i: u64) void = {
	_abort(path, line, col, reasons[i]);
};

fn u64tos(u: u64) (*const u8, size) = {
	static let buf: [20]u8 = [0...]; // len("18446744073709551615")
	let sl = buf[..0];
	if (u == 0) {
		static append(sl, '0');
	};
	for (u > 0) {
		static append(sl, (u % 10): u8 + '0');
		u /= 10;
	};
	for (let s = 0z, e = len(sl) - 1; s < e) {
		let tmp = sl[s];
		sl[s] = sl[e];
		sl[e] = tmp;
		s += 1;
		e -= 1;
	};
	return (sl: *[*]u8: *const u8, len(sl));
};
