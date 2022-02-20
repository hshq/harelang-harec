export type slice = struct {
	data: nullable *void,
	length: size,
	capacity: size,
};

export fn ensure(s: *slice, membsz: size) void = {
	let cap = s.capacity;
	if (cap >= s.length) {
		return;
	};
	for (cap < s.length) {
		assert(cap >= s.capacity, "slice out of memory (overflow)");
		if (cap == 0) {
			cap = s.length;
		} else {
			cap *= 2;
		};
	};
	s.capacity = cap;
	const data = realloc(s.data, s.capacity * membsz);
	assert(data != null || s.capacity * membsz == 0);
	s.data = data;
};

export fn unensure(s: *slice, membsz: size) void = {
	let cap = s.capacity;
	for (cap > s.length) {
		cap /= 2;
	};
	cap *= 2;
	s.capacity = cap;
	const data = realloc(s.data, s.capacity * membsz);
	assert(data != null || s.capacity * membsz == 0);
	s.data = data;
};

type base = enum uint {
	BIN = 2,
	OCT = 8,
	DEC = 10,
	HEX = 16,
};

// XXX: TEMP
fn u64tosb(u: u64, b: base) const str = {
	static let buf: [64]u8 = [0...]; // 64 binary digits

	static const lut_upper = [
		'0', '1', '2', '3', '4', '5', '6', '7',
		'8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
	];
	const lut = &lut_upper;

	let s = string { data = &buf, ... };
	if (u == 0) {
		buf[s.length] = '0': u32: u8;
		s.length += 1z;
	};

	for (u > 0u64) {
		buf[s.length] = lut[u % b: u64]: u32: u8;
		s.length += 1;
		u /= b;
	};

	reverse(buf[..s.length]);
	return *(&s: *str);
};

fn reverse(b: []u8) void = {
	if (len(b) == 0) {
		return;
	};
	for (let s = 0z, e = len(b) - 1; s < e) {
		let x = b[s];
		b[s] = b[e];
		b[e] = x;
		s += 1;
		e -= 1;
	};
};

fn println(msgs: str...) void = {
	for (let i = 0z; i < len(msgs); i += 1) {
		let msg = msgs[i];
		write(1, *(&msg: **void): *const char, len(msg)): void;
		if (i + 1 < len(msgs)) {
			const sp = " ";
			write(1, *(&sp: **void): *const char, 1): void;
		};
	};
	const linefeed = "\n";
	write(1, *(&linefeed: **void): *const char, 1): void;
};
