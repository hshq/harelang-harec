export fn memcmp(a: *void, b: *void, amt: size) bool = {
	let _a = a: *[*]u8, _b = b: *[*]u8;
	for (let i = 0z; i < amt; i += 1z) {
		if (a[i] != b[i]) {
			return false;
		};
	};
	return true;
};
