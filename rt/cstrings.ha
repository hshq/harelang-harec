type string = struct {
	data: nullable *[*]u8,
	length: size,
	capacity: size,
};

export fn toutf8(s: str) []u8 = *(&s: *[]u8);

fn constchar(s: str) *const char = {
	let s = &s: *string;
	return s.data: *const char;
};

fn alloc_constchar(s: str) *const char = {
	let c: []char = alloc([], len(s) + 1);
	append(c, *(&s: *[]char)...);
	append(c, 0);
	return constchar(*(&c: *str));
};
