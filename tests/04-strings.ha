fn measurements() void = {
	let x = "Hello!";
	assert(len(x) == 6z);
};

fn charptr() void = {
	let x = "Hello!";
	let y = x: *const char;
	let z = y: *[*]u8;
	// XXX: Update me when implicit deref is implemented
	assert((*z)[0] == 'H': u32: u8);
	assert((*z)[1] == 'e': u32: u8);
	assert((*z)[2] == 'l': u32: u8);
	assert((*z)[3] == 'l': u32: u8);
	assert((*z)[4] == 'o': u32: u8);
	assert((*z)[5] == '!': u32: u8);
};

export fn main() void = {
	// TODO: Expand this test (blocked on structs, more or less)
	measurements();
	charptr();
};
