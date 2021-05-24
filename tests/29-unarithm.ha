type abool = bool;

fn lnot() void = {
	assert(!(false: abool));
};

export fn main() void = {
	lnot();
};
