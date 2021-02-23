type err_int = int!;

fn assignability() void = {
	// Error and non-error types are interchangable:
	let a: int! = 10;
	let b: int = a;
	assert(a == b);
};

export fn main() void = {
	assignability();
	// TODO: Expand with tests for the ? operator
};
