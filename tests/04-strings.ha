fn measurements() void = {
	const x = "Hello!";
	assert(len(x) == 6z);
	assert(size(str) == size(*u8) + size(size) * 2z);
	const align: size =
		if (size(*u8) > size(size)) size(*u8)
		else size(size);
	assert(&x: uintptr: size % align == 0z);
};

fn charptr() void = {
	const x = "Hello!";
	const y = x: *const char;
	const z = y: *[*]u8;
	assert(z[0] == 'H': u32: u8);
	assert(z[1] == 'e': u32: u8);
	assert(z[2] == 'l': u32: u8);
	assert(z[3] == 'l': u32: u8);
	assert(z[4] == 'o': u32: u8);
	assert(z[5] == '!': u32: u8);
	assert(z[6] == 0u8);
};

fn storage() void = {
	const string = "Hello!";
	const ptr = &string: *struct {
		data: *[*]u8,
		length: size,
		capacity: size,
	};
	assert(ptr.length == 6z);
	assert(ptr.capacity == 6z);
	assert(ptr.data[0] == 'H': u32: u8);
	assert(ptr.data[1] == 'e': u32: u8);
	assert(ptr.data[2] == 'l': u32: u8);
	assert(ptr.data[3] == 'l': u32: u8);
	assert(ptr.data[4] == 'o': u32: u8);
	assert(ptr.data[5] == '!': u32: u8);
	assert(ptr.data[6] == 0u8);
};

export fn main() void = {
	measurements();
	charptr();
	storage();
};
