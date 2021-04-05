fn equality() void = {
	assert((if (2 == 2) true else false));
};

fn inequality() void = {
	assert((if (2 == 1) false else true));
};

fn else_() void = {
	assert((if (2 == 1) false else true));
};

fn elseif() void = {
	assert((if (2 == 1) false
		else if (2 == 2) true
		else false));
};

fn justif() void = {
	let x: int = 1;
	if (true) {
		// asserts that the side-effects work
		x = 0;
	};
	assert(x == 0);
};

fn gt() void = {
	assert((if (1 > 2) false else true));
};

fn gte() void = {
	assert((if (2 >= 2) true else false));
};

fn lt() void = {
	assert((if (1 < 2) true else false));
};

fn lte() void = {
	assert((if (2 <= 2) true else false));
};

fn and2(left: bool) bool = {
	return if (left && 1 / 0 == 0) false else true;
};

fn and() void = {
	assert((if (true && and2(1 == 1 && false)) true else false));
};

fn or() void = {
	assert((if (true || 1 / 0 == 0) true else false));
};

fn tagged() void = {
	assert((if (true) 1u8 else 0i8) as u8 == 1); 
	assert((if (false) 1u8 else 0i8) as i8 == 0); 
};

type abool = bool;

fn alias() void = {
	if (true: abool) {
		return;
	};
	abort("unreachable");
};

export fn main() void = {
	equality();
	inequality();
	else_();
	elseif();
	justif();
	gt();
	gte();
	lt();
	lte();
	and();
	or();
	tagged();
	alias();
};
