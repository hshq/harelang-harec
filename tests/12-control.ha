fn rt::compile(src: str) int;

export fn main() void = {
	for (let i: int = 0; i < 1; i += 1) {
		let j: int = 0;
		for (j < 10) {
			j += 1;
			if (j == 5) {
				break;
			};
		};
		assert(j == 5, "Failed to break out of while");
	};
	let done: bool = false;
	for (!done) {
		for (let i: int = 0; i < 10; i += 1) {
			if (i == 5) {
				true;
				continue;
			};
			assert(i != 5, "Broken continue");
			if (i >= 7) {
				break;
			};
			assert(i < 7, "Broken break");
		};
		done = true;
	};
	let i = 0;
	:outer for (i < 10) {
		:inner for (let j = 0; j < 7; j += 1) {
			:outer for (true) {
				break :outer;
			};
			i += 1;
			if (j == 6) {
				continue :inner;
			};
			assert(j != 6);
			if (i > 7) {
				break :outer;
			};
		};
	};
	assert(i == 8);
	assert(rt::compile("fn test() void = { :foo for (true) { break :bar; }; };") != 0);
	assert(rt::compile("fn test() void = { break :bar; };") != 0);
};
