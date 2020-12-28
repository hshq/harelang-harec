export fn memzero(ptr: *void, amt: size) void = {
	let a = ptr: *[*]u8;
	for (let i = 0z; i < amt; i += 1) {
		a[i] = 0u8;
	};
};
