use rt;
use test = rt;
use rt::{malloc, free_};

export fn main() void = {
	assert(&rt::malloc == &test::malloc);
	assert(&rt::malloc == &malloc);
	assert(&rt::free_ == &test::free_);
	assert(&rt::free_ == &free_);
};
