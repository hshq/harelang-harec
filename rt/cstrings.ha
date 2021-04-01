type string = struct {
	data: nullable *[*]u8,
	length: size,
	capacity: size,
};

fn constchar(s: str) *const char = {
	let s = &s: *string;
	return s.data: *const char;
};
