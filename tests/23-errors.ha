use rt::{compile, status};

type err_int = !int;

fn assignability() void = {
	// Error and non-error types are interchangable:
	let a: !int = 10;
	let b: int = a;
	assert(a == b);
};

type error = !void;

fn err_if_false(in: bool) (error | int) = {
	if (in) {
		return 1337;
	};
	return error;
};

fn indirect(in: bool) (error | int) = {
	let x = err_if_false(in)?;
	return x;
};

fn propagate() void = {
	assert(indirect(true) as int == 1337);
	assert(indirect(false) is error);
};

fn cannotignore() void = {
	compile(status::CHECK, "
		type error = !void;

		export fn main() int = {
			error;
			return 42;
		};
	")!;
	err_if_false(true)!;
};

fn void_assignability() void = {
	compile(status::CHECK, `
		type err = !void;

		fn reterr() (int | err) = {
			return err;
		};

		fn properr() void = {
			reterr()?;
		};

		export fn main() void = void;
	`)!; // error types cannot be assigned to void

	compile(status::CHECK, `
		fn disallow_1() void = {
			return "I am illegal";
		};

		fn disallow_2() void = {
			return 12;
		};

		export fn main() void = void;
	`)!; // non-void types cannot be assigned to void
};

fn measurements() void = {
	assert(size(!int) == size(int));
	assert(size(!f64) == size(f64));
	assert(size(!(int | void)) == size((int | void)));
	assert(size(!(i8, rune)) == size((i8, rune)));
	assert(size(!struct { x: int, y: str }) == size(struct { x: int, y: str }));
	assert(size(!union { x: int, y: str }) == size(union { x: int, y: str }));
	assert(size(![2]int) == size([2]int));
	assert(size(![]int) == size([]int));
	assert(size(!*size) == size(*size));

	assert(align(!int) == align(int));
	assert(align(!f64) == align(f64));
	assert(align(!(int | void)) == align((int | void)));
	assert(align(!(i8, rune)) == align((i8, rune)));
	assert(align(!struct { x: int, y: str }) == align(struct { x: int, y: str }));
	assert(align(!union { x: int, y: str }) == align(union { x: int, y: str }));
	assert(align(![2]int) == align([2]int));
	assert(align(![*]int) == align([*]int));
	assert(align(![]int) == align([]int));
	assert(align(!*size) == align(*size));
};

export fn main() void = {
	assignability();
	propagate();
	cannotignore();
	void_assignability();
	measurements();
};
