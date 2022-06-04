export type _enum = enum {
	ONE = 1,
	TWO = 2,
	THREE = 3,
};

export type other = enum {
	// purposefully something that doesn't exist in _enum
	EIGHT = 8: _enum,
};

export type char_enum = enum char {
	ONE = 1,
};

export type enum_alias = _enum;

export def val: int = 42;
export def val2: int = 90;
