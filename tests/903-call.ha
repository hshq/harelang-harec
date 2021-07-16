fn foo() size = 2;

export fn main() int = {
	// Indirect
	let x: size = foo();
	assert(x == 2);

	// Direct
	let x = [1, 2, 3];
	assert(x[foo()] == 3);
	return 0;
};
