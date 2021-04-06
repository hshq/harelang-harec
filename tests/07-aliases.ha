type my_int = int;

fn alias_builtin() void = {
	let i: my_int = 1234;
	assert(i == 1234, "built-in alias");
};

fn unwrap() void = {
	let i: ...my_int = 1234;
	assert(i == 1234);
};

type my_array = [3]int;
type my_array_ptr = *my_array;
type my_array_ptr_ptr = *my_array_ptr;
type my_slice = []int;

fn alias_array() void = {
	let a: my_array = [1, 2, 3];
	let i: my_int = 0;
	let b: my_array_ptr = &a;
	let c: my_array_ptr_ptr = &b;
	let d: my_slice = c[..];
	assert(a[i] == 1, "array alias");
	assert(a[1] == 2, "array alias");
	assert(a[2] == 3, "array alias");
	assert(b[i] == 1, "array ptr alias");
	assert(b[1] == 2, "array ptr alias");
	assert(b[2] == 3, "array ptr alias");
	assert(c[i] == 1, "array ptr ptr alias");
	assert(c[1] == 2, "array ptr ptr alias");
	assert(c[2] == 3, "array ptr ptr alias");
	assert(d[i] == 1, "array ptr ptr slice alias");
	assert(d[1] == 2, "array ptr ptr slice alias");
	assert(d[2] == 3, "array ptr ptr slice alias");
};

type my_fn = *const fn(_: int) int;
type my_fn_ptr = *my_fn;
type my_fn_ptr_ptr = *my_fn_ptr;

fn foo(n: int) int = (n + 1) * 2;

fn alias_fn() void = {
	let f: my_fn = &foo;
	let g: my_fn_ptr = &f;
	let h: my_fn_ptr_ptr = &g;
	assert(f(0) == foo(0), "fn alias");
	assert(g(0) == foo(0), "fn ptr alias");
	assert(h(0) == foo(0), "fn ptr ptr alias");
};

type my_struct = struct { x: int, y: int };
type my_struct_ptr = *my_struct;
type my_struct_ptr_ptr = *my_struct_ptr;
type my_other_struct = struct { x: my_struct_ptr_ptr };

fn alias_struct() void = {
	let s: my_struct = struct {
		x: int = 42,
		y: int = 69,
	};
	let t: my_struct_ptr = &s;
	let u: my_struct_ptr_ptr = &t;
	let v: my_other_struct = struct { x: my_struct_ptr_ptr = u };
	assert(s.x == 42, "struct alias");
	assert(s.y == 69, "struct alias");
	assert(t.x == 42, "struct alias ptr");
	assert(t.y == 69, "struct alias ptr");
	assert(u.x == 42, "struct alias ptr ptr");
	assert(u.y == 69, "struct alias ptr ptr");
	assert(v.x.x == 42, "struct alias ptr ptr alias");
	assert(v.x.y == 69, "struct alias ptr ptr alias");
};

type my_tagged = (int | void);

fn alias_tagged() void = {
	let a: my_tagged = 42;
	assert(a is int, "tag");
	assert(a as int == 42, "value");
};

type my_my_array = my_array;
type my_my_int = my_int;

fn alias_alias() void = {
	let a: my_my_array = [1, 2, 3];
	let i: my_my_int = 0;
	assert(a[i] == 1, "alias alias");
	assert(a[1] == 2, "alias alias");
	assert(a[2] == 3, "alias alias");
};

type recur = struct {
	self: *recur,
};

fn recursive() void = {
	let x: recur = struct {
		self: *recur = &x,
	};
	x.self = &x;
	assert(x.self == x.self.self);
};

export fn main() void = {
	alias_builtin();
	alias_array();
	alias_fn();
	alias_struct();
	alias_tagged();
	alias_alias();
	recursive();
};
