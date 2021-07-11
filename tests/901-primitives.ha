export fn main() int = {
	let a: int = 10;
	assert(a == 10);
	a = 20;
	assert(a == 20);

	let a: u8 = 5u8;
	assert(a == 5u8);
	a = 10u8;
	assert(a == 10u8);

	return 0;
};
