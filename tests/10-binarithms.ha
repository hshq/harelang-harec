
fn error() bool = {
	abort();
	return false;
};

fn set(x: *int) bool = {
	*x = 42;
	return true;
};

fn andor() void = {
	assert((false || false) == false);
	assert((false || true) == true);
	assert((true || false) == true);
	assert((true || true) == true);
	assert((true || error()) == true);
	let x = 0;
	assert((false || set(&x)) == true);
	assert(x == 42);

	assert((false && false) == false);
	assert((false && true) == false);
	assert((true && false) == false);
	assert((true && true) == true);
	assert((false && error()) == false);
	x = 0;
	assert((true && set(&x)) == true);
	assert(x == 42);
};

export fn main() void = {
	// TODO: other binarithms
	andor();
};
