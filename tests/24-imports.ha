use rt;

fn accept() void = {
	assert(rt::compile("
		use rt;
		export fn main() void = { &rt::free_; };
	") == 0);
	assert(rt::compile("
		use rt;
		use test = rt;
		export fn main() void = static assert(&rt::free_ == &test::free_);
	") == 0);
	assert(rt::compile("
		use rt;
		use rt::{free_, malloc};
		export fn main() void = static assert(
			&rt::free_ == &free_ && &rt::malloc == &malloc
		);
	") == 0);
	assert(rt::compile("
		use rt;
		use rt::*;
		export fn main() void = static assert(
			&rt::free_ == &free_ && &rt::malloc == &malloc
		);
	") == 0);
	assert(rt::compile("
		use rt;
		use rt::{test = free_, malloc};
		export fn main() void = static assert(
			&rt::free_ == &test && &rt::malloc == &malloc
		);
	") == 0);
	assert(rt::compile("
		use rt;
		use test = rt::{test = free_, malloc};
		export fn main() void = static assert(
			&rt::free_ == &test::test && &rt::malloc == &test::malloc
		);
	") == 0);
};

fn reject() void = {
	assert(rt::compile("
		use wrong;
		export fn main() void = &rt::free_;
	") != 0);
	assert(rt::compile("
		use rt::{free_};
		export fn main() void = static assert(
			&rt::free_ != null
		);
	") != 0);
	assert(rt::compile("
		use rt::{free_};
		export fn main() void = static assert(
			&malloc != null
		);
	") != 0);
	assert(rt::compile("
		use rt;
		use test = rt::*;
		export fn main() void = void;
	") != 0);
	assert(rt::compile("
		use rt;
		use rt*;
		export fn main() void = void;
	") != 0);
	assert(rt::compile("
		use rt::{test = free_, malloc};
		export fn main() void = static assert(&free_ != null);
	") != 0);
	assert(rt::compile("
		use test = rt::{test = free_, malloc};
		export fn main() void = static assert(&test != null);
	") != 0);
};


export fn main() void = {
	accept();
	reject();
};
