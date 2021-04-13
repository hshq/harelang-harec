export fn main() void = {
	let i1 = 13, i2 = 13i, i3 = 13i8, i4 = 13i16, i5 = 13i32, i6 = 13i64;
	let u1 = 13u, u2 = 13z, u3 = 13u8, u4 = 13u16, u5 = 13u32, u6 = 13u64;
	let n = -13;
	let b1 = true, b2 = false;
	let p1: nullable *int = null;
	let r1 = 'x', r2 = '\x0A', r3 = '\u1234', r4 = '\0', r5 = '\a',
		r6 = '\b', r7 = '\f', r8 = '\n', r9 = '\r', r10 = '\t',
		r11 = '\v', r12 = '\\', r13 = '\'', r14 = '\"';
	let f1 = 1.0, f2 = 1f32, f3 = 1.0e2, f4 = 1.0f64;
	let f5 = 1.23e+45, f6 = 9.87e-65, f7 = 1e-7;
};
