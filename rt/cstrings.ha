type string = struct {
	data: nullable *[*]u8,
	length: size,
	capacity: size,
};

export fn toutf8(s: str) []u8 = *(&s: *[]u8);

fn constchar(s: str) *const u8 = {
	let s = &s: *string;
	return s.data: *const u8;
};

fn alloc_constchar(s: str) *const u8 = {
	let c: []u8 = alloc([], len(s) + 1);
	append(c, *(&s: *[]u8)...);
	append(c, 0);
	return constchar(*(&c: *str));
};
