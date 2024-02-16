use rt::{compile, status, toutf8};

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
		assert(t[i] == expected[i]: u8);
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

fn escapes() void = {
	const s = "à";
	assert(s == "\xc3\xa0");
	assert(s == "\xc3" "" "\xa0");
	assert(s == "\u00e0");
	assert(s == "\U000000e0");
	const s = toutf8(s);
	assert(len(s) == 2 && s[0] == 0xc3 && s[1] == 0xa0);

	assert("\x345" == "45");
	assert("\033" == "\x0033");
};

fn raw() void = {
	assert(`hello \" world` == "hello \\\" world");
};

fn reject() void = {
	compile(status::CHECK, `
		fn f() void = {
			let x = "asdf" + "fdsa";
		};
	`)!;
	compile(status::CHECK, `
		fn f() void = {
			let x = "asdf";
			x += "fdsa";
		};
	`)!;

	compile(status::PARSE, `let s = "\xc3";`)!;
	compile(status::PARSE, `let s = "\xc3\x00";`)!;
	compile(status::PARSE, `let s = "\xc30";`)!;
	compile(status::PARSE, `let s = "\xc3" "\xc3";`)!;
	compile(status::LEX, `let s = "\xa";`)!;
	compile(status::LEX, `let s = "\u69";`)!;
	compile(status::LEX, `let s = "\U1234567";`)!;
	compile(status::LEX, `let s = "\xah";`)!;
	compile(status::LEX, `let s = "\uahij";`)!;
	compile(status::LEX, `let s = "\Uahijklmn";`)!;
};

export fn main() void = {
	measurements();
	storage();
	concat();
	equality();
	escapes();
	raw();
	reject();
};
