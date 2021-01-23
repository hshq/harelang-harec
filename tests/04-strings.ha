fn measurements() void = {
	const x = "Hello!";
	assert(len(x) == 6z);
	assert(len("Hello!") == 6z);
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
	const expected = ['H', 'e', 'l', 'l', 'o', '!', '\0'];
	for (let i = 0z; i < len(expected); i += 1z) {
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
	assert(ptr.length == 15z && ptr.capacity == 15z);

	// UTF-8 encoded
	const expected = [
		0xE3u8, 0x81u8, 0x93u8, 0xE3u8, 0x82u8, 0x93u8, 0xE3u8, 0x81u8,
		0xABu8, 0xE3u8, 0x81u8, 0xA1u8, 0xE3u8, 0x81u8, 0xAFu8, 0x00u8,
	];
	for (let i = 0z; i < len(expected); i += 1z) {
		assert(ptr.data[i] == expected[i]);
	};
};

fn concat() void = {
	const s = "Hell" "o, " "wor" "ld!";
	const t = s: *const char: *[*]u8;
	const expected = [
		'H', 'e', 'l', 'l', 'o', ',', ' ',
		'w', 'o', 'r', 'l', 'd', '!', '\0',
	];
	for (let i = 0z; i < len(expected); i += 1z) {
		assert(t[i] == expected[i]: u32: u8);
	};
};

export fn main() void = {
	measurements();
	charptr();
	storage();
	concat();
};
