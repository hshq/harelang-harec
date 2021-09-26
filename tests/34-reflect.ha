use types;
use types::{builtin};

fn builtins() void = {
	const cases: [_](type, size, size, builtin) = [
		(type(u8), 1, 1, builtin::U8),
		(type(u16), 2, 2, builtin::U16),
		(type(u32), 4, 4, builtin::U32),
		(type(u64), 8, 8, builtin::U64),
		(type(i8), 1, 1, builtin::I8),
		(type(i16), 2, 2, builtin::I16),
		(type(i32), 4, 4, builtin::I32),
		(type(i64), 8, 8, builtin::I64),
		(type(f32), 4, 4, builtin::F32),
		(type(f64), 8, 8, builtin::F64),
		(type(rune), 4, 4, builtin::RUNE),
		(type(char), 1, 1, builtin::CHAR),
		(type(void), 0, 0, builtin::VOID),
		(type(str), size(str), size(uintptr), builtin::STR),
		(type(bool), size(uint), size(uint), builtin::BOOL),
		(type(int), size(int), size(int), builtin::INT),
		(type(uint), size(uint), size(uint), builtin::UINT),
		(type(size), size(size), size(size), builtin::SIZE),
		(type(uintptr), size(uintptr), size(uintptr), builtin::UINTPTR),
		(type(type), size(type), size(type), builtin::TYPE), // so meta
	];
	for (let i = 0z; i < len(cases); i += 1) {
		const item = cases[i];
		let t = types::reflect(item.0);
		assert(t.sz == item.1);
		assert(t.al == item.2);
		let b = t.repr as builtin;
		assert(b == item.3);
	};
};

type alias = i32;

fn aliases() void = {
	let t = types::reflect(type(alias));
	assert(t.sz == 4);
	assert(t.al == 4);
	let a = t.repr as types::alias;
	assert(a.secondary == type(i32));
	assert(len(a.ident) == 1);
	assert(a.ident[0] == "alias");
};

type watchmen = enum {
	VIMES,
	CARROT,
	ANGUA,
	COLON,
	NOBBY = -1,
};

fn enums() void = {
	let ty = types::unwrap(type(watchmen));
	let em = ty.repr as types::enumerated;
	assert(em.storage == builtin::INT);

	const cases: [_](str, u64) = [
		("VIMES", 0),
		("CARROT", 1),
		("ANGUA", 2),
		("COLON", 3),
		("NOBBY", -1: u64),
	];
	for (let i = 0z; i < len(em.values); i += 1) {
		const val = em.values[i];
		const expect = cases[i];
		assert(val.0 == expect.0 && val.1 == expect.1);
	};
};

fn arrays() void = {
	let ty = types::reflect(type([42]int));
	assert(ty.sz == 42 * size(int));
	assert(ty.al == size(int));
	let ar = ty.repr as types::array;
	assert(ar.length == 42);
	assert(ar.members == type(int));
};

fn slices() void = {
	let ty = types::reflect(type([]int));
	assert(ty.sz == size(*int) + size(size) + size(size));
	assert(ty.al == size(*int));
	let sl = ty.repr as types::slice_repr;
	assert(sl == type(int));
};

fn pointers() void = {
	let ty = types::reflect(type(*int));
	assert(ty.sz == size(*int));
	assert(ty.al == size(*int));
	let pt = ty.repr as types::pointer;
	assert(pt.secondary == type(int));
	assert(pt.flags == 0);

	let ty = types::reflect(type(nullable *int));
	assert(ty.sz == size(*int));
	assert(ty.al == size(*int));
	let pt = ty.repr as types::pointer;
	assert(pt.secondary == type(int));
	assert(pt.flags == types::pointer_flags::NULLABLE);
};

fn functions() void = {
	let ty = types::reflect(type(fn(x: int, y: int) int));
	assert(ty.sz == types::SIZE_UNDEFINED);
	assert(ty.al == types::SIZE_UNDEFINED);
	let func = ty.repr as types::func;
	assert(func.result == type(int));
	assert(func.variadism == types::variadism::NONE);
	assert(func.flags == 0);
	assert(len(func.params) == 2);
	assert(func.params[0] == type(int));
	assert(func.params[1] == type(int));

	let ty = types::reflect(type(@noreturn fn(x: int, y: int, z: int...) int));
	let func = ty.repr as types::func;
	assert(func.variadism == types::variadism::HARE);
	assert(func.flags == types::func_flags::NORETURN);
};

fn struct_union() void = {
	let ty = types::reflect(type(struct {
		x: int,
		y: int,
	}));
	assert(ty.sz == size(int) * 2);
	assert(ty.al == size(int));
	let su = ty.repr as types::struct_union;
	assert(su.kind == types::struct_kind::STRUCT);
	assert(len(su.fields) == 2);
	assert(su.fields[0].name == "x");
	assert(su.fields[0].offs == 0);
	assert(su.fields[0].type_ == type(int));
	assert(su.fields[1].name == "y");
	assert(su.fields[1].offs == 4);
	assert(su.fields[1].type_ == type(int));

	let ty = types::reflect(type(union {
		x: int,
		y: int,
	}));
	let su = ty.repr as types::struct_union;
	assert(su.kind == types::struct_kind::UNION);
	assert(len(su.fields) == 2);
	assert(su.fields[0].name == "x");
	assert(su.fields[0].offs == 0);
	assert(su.fields[1].name == "y");
	assert(su.fields[1].offs == 0);

	let ty = types::reflect(type(struct {
		x: int,
		y: int,
		struct {
			z: int,
		},
	}));
	let su = ty.repr as types::struct_union;
	assert(len(su.fields) == 3);
	assert(su.fields[2].name == "");
	assert(su.fields[2].offs == 8);
};

export fn main() void = {
	builtins();
	aliases();
	enums();
	arrays();
	slices();
	pointers();
	functions();
	struct_union();
};
