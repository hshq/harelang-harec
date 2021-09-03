// This defines a subset of types/reflect.ha so we can declare static typeinfo
// structures for builtins.
type types::typeinfo = struct {
	id: uint,
	sz: size,
	al: size,
	flags: types::flags,
	repr: types::repr,
};

type types::flags = enum u8 {
	NONE = 0,
	CONST = 1 << 0,
	ERROR = 1 << 1,
};

type types::repr = (types::builtin | void);

type types::builtin = enum u8 {
	BOOL, CHAR, F32, F64, I16, I32, I64, I8, INT, NULL, RUNE, SIZE, STR,
	U16, U32, U64, U8, UINT, UINTPTR, VOID,
};

export const builtin_char: types::typeinfo = types::typeinfo {
	id = 3950255460,
	sz = 1, al = 1, flags = 0,
	repr = types::builtin::CHAR,
}, builtin_f32: types::typeinfo = types::typeinfo {
	id = 930681398,
	sz = 4, al = 4, flags = 0,
	repr = types::builtin::F32,
}, builtin_f64: types::typeinfo = types::typeinfo {
	id = 2037165609,
	sz = 8, al = 8, flags = 0,
	repr = types::builtin::F64,
}, builtin_i8: types::typeinfo = types::typeinfo {
	id = 461893804,
	sz = 1, al = 1, flags = 0,
	repr = types::builtin::I8,
}, builtin_i16: types::typeinfo = types::typeinfo {
	id = 3312558843,
	sz = 2, al = 2, flags = 0,
	repr = types::builtin::I16,
}, builtin_i32: types::typeinfo = types::typeinfo {
	id = 2674862226,
	sz = 4, al = 4, flags = 0,
	repr = types::builtin::I32,
}, builtin_i64: types::typeinfo = types::typeinfo {
	id = 1099590421,
	sz = 8, al = 8, flags = 0,
	repr = types::builtin::I64,
}, builtin_rune: types::typeinfo = types::typeinfo {
	id = 2206074632,
	sz = 4, al = 4, flags = 0,
	repr = types::builtin::RUNE,
}, builtin_u8: types::typeinfo = types::typeinfo {
	id = 3181589295,
	sz = 1, al = 1, flags = 0,
	repr = types::builtin::U8,
}, builtin_u16: types::typeinfo = types::typeinfo {
	id = 3481467866,
	sz = 2, al = 2, flags = 0,
	repr = types::builtin::U16,
}, builtin_u32: types::typeinfo = types::typeinfo {
	id = 1906196061,
	sz = 4, al = 4, flags = 0,
	repr = types::builtin::U32,
}, builtin_u64: types::typeinfo = types::typeinfo {
	id = 1268499444,
	sz = 8, al = 8, flags = 0,
	repr = types::builtin::U64,
}, builtin_void: types::typeinfo = types::typeinfo {
	id = 3012680272,
	sz = 0, al = 0, flags = 0,
	repr = types::builtin::VOID,
};
