fn measurements() void = {
	const x = "Hello!";
	assert(len(x) == 6);
	assert(len("Hello!") == 6);
	assert(size(str) == size(*u8) + size(size) * 2);
	const align: size =
		if (size(*u8) > size(size)) size(*u8)
		else size(size);
	assert(&x: uintptr: size % align == 0);
};

fn charptr() void = {
	const x = "Hello!";
	const y = x: *const char;
	const z = y: *[*]u8;
	const expected = ['H', 'e', 'l', 'l', 'o', '!', '\0'];
	for (let i = 0z; i < len(expected); i += 1) {
		assert(z[i] == expected[i]: u32: u8);
	};
};

fn storage() void = {
	const string = "こんにちは";
	const ptr = &string: *struct {
		data: *[*]u8,
		length: size,
		capacity: size,
	};
	assert(ptr.length == 15 && ptr.capacity == 15);

	// UTF-8 encoded
	const expected: [_]u8 = [
		0xE3, 0x81, 0x93, 0xE3, 0x82, 0x93, 0xE3, 0x81,
		0xAB, 0xE3, 0x81, 0xA1, 0xE3, 0x81, 0xAF, 0x00,
	];
	for (let i = 0z; i < len(expected); i += 1) {
		assert(ptr.data[i] == expected[i]);
	};

	const empty = "";
	const ptr2 = &empty: *struct {
		data: nullable *[*]u8,
		length: size,
		capacity: size,
	};
	assert(ptr2.data == null);
};

fn concat() void = {
	const s = "Hell" "o, " "wor" "ld!";
	const t = s: *const char: *[*]u8;
	const expected = [
		'H', 'e', 'l', 'l', 'o', ',', ' ',
		'w', 'o', 'r', 'l', 'd', '!', '\0',
	];
	for (let i = 0z; i < len(expected); i += 1) {
		assert(t[i] == expected[i]: u32: u8);
	};
};

fn equality() void = {
	assert("foo" != "bar");
	assert("foo" != "foobar");
	assert("foobar" == "foobar");
};

export fn main() void = {
	measurements();
	charptr();
	storage();
	concat();
	equality();
};
