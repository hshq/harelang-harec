// tests for the test runtime itself

use rt;

fn compile() void = {
	assert(rt::compile("
		fn test() void = {
			void;
		};"
	) as rt::exited == rt::EXIT_SUCCESS);
	assert(rt::compile("
		fn test() void = {
			let a: int = [1, 2, 3];
		};"
	) as rt::exited != rt::EXIT_SUCCESS);
};

let global = 0i;

fn foofn() str = {
	global += 1;
	return "foo";
};

def FOO: str = "foo";

type s = str;
type b = bool;

static assert(true);
static assert(true, "msg");

fn assert_() void = {
	let foo = "foo";
	static assert(true, "foo");
	static assert(true: b, "foo");
	static assert(true, "foo": s);
	static assert(true, FOO);
	static assert(true: b, FOO);
	static assert(true, FOO: s);

	assert(true, "foo");
	assert(true: b, "foo");
	assert(true, "foo": s);
	assert(true, FOO);
	assert(true: b, FOO);
	assert(true, FOO: s);
	assert(true, foo);
	assert(true: b, foo);
	assert(true, foo: s);
	assert(true, foofn());
	assert(true: b, foofn());
	assert(true, foofn(): s);

	assert(global == 0); // no side effects if assertion succeeds

	let failures: [_]str = [
		// types
		"fn f() void = { assert(5); };",
		"fn f() void = { assert(true, 5); };",
		"fn f() void = { static assert(5); };",
		"fn f() void = { static assert(true, 5); };",

		// compile-time eval
		"fn f() void = { let a = true; static assert(a); };",
		"fn f() void = { let a = \"foo\"; static assert(true, a); };",

		// top-level static assertions
		"static assert(false);",
		"export static assert(true);",
	];

	for (let i = 0z; i < len(failures); i += 1) {
		assert(rt::compile(failures[i]) as rt::exited != rt::EXIT_SUCCESS);
	};
};

export fn main() void = {
	assert_();
	compile();
};
