use rt::{compile, exited, EXIT_SUCCESS};

fn measurements() void = {
	const x = "Hello!";
	assert(len(x) == 6);
	assert(len("Hello!") == 6);
	assert(len("Hello!\0") == 7);
	assert(len("He\0llo!") == 7);
	assert(size(str) == size(*u8) + size(size) * 2);
	const alignment: size =
		if (size(*u8) > size(size)) size(*u8)
		else size(size);
	assert(align(str) % alignment == 0);

	static assert(len("Hello!") == 6);
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
		0xAB, 0xE3, 0x81, 0xA1, 0xE3, 0x81, 0xAF,
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
	const t = *(&s: **[*]u8);
	const expected = [
		'H', 'e', 'l', 'l', 'o', ',', ' ',
		'w', 'o', 'r', 'l', 'd', '!',
	];
	for (let i = 0z; i < len(expected); i += 1) {
		assert(t[i] == expected[i]: u32: u8);
	};
};

fn equality() void = {
	assert("foo" != "bar");
	assert("foo" != "foobar");
	assert("foobar" == "foobar");
	assert("foo\0bar" != "foo\0foo");
	static assert("foo" != "bar");
	static assert("foo" != "foobar");
	static assert("foobar" == "foobar");
	static assert("foo\0bar" != "foo\0foo");
};

fn raw() void = {
	assert(`hello \" world` == "hello \\\" world");
};

fn reject() void = {
	assert(compile(`
		fn f() void = {
			let x = "asdf" + "fdsa";
		};
	`) as exited != EXIT_SUCCESS);
	assert(compile(`
		fn f() void = {
			let x = "asdf";
			x += "fdsa";
		};
	`) as exited != EXIT_SUCCESS);
};

export fn main() void = {
	measurements();
	storage();
	concat();
	equality();
	raw();
	reject();
};
