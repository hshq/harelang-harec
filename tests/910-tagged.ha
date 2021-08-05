fn totagged() void = {
	// Simple case
	let x: (int | void) = void;
	let p = &x: *struct {
		id: uint,
		data: int,
	};
	assert(p.id == 3012680272);
	x = 1337;
	assert(p.id == 1737287038);
	assert(p.data == 1337);

	// Align of 4
	let x: (int | f32 | void) = 1337;
	let p = &x: *struct {
		id: uint,
		data: union {
			idata: int,
			fdata: f32,
		},
	};
	assert(p.id == 1737287038);
	assert(p.data.idata == 1337);
	x = 13.37f32;
	assert(p.id == 930681398);
	assert(p.data.fdata == 13.37f32);

	// Align of 8
	let x: (size | void) = 1337z;
	let p = &x: *struct {
		id: uint,
		data: size,
	};
	assert(p.id == 4119164483);
	assert(p.data == 1337z);

	// TODO: More cases
};

export fn main() int = {
	totagged();
	return 0;
};
