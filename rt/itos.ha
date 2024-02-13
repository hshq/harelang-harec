// SPDX-License-Identifier: MPL-2.0
// (c) Hare authors <https://harelang.org>

fn itos(i: int) str = {
	static let buf: [len("-2147483648")]u8 = [0...];
	let sl = buf[..0];
	if (i == 0) {
		return "0";
	} else if (i == -2147483648) {
		return "-2147483648";
	};
	if (i < 0) {
		static append(sl, '-');
		i = -i;
	};
	for (i > 0) {
		static append(sl, (i % 10): u8 + '0');
		i /= 10;
	};
	for (let s = 0z, e = len(sl) - 1; s < e) {
		let tmp = sl[s];
		sl[s] = sl[e];
		sl[e] = tmp;
		s += 1;
		e -= 1;
	};
	return *(&sl: *str);
};
