type implicit = enum {
	ZERO,
	ONE,
	TWO,
};

type explicit = enum {
	NEGATIVE = -1,
	FORTY_TWO = 42,
	FORTY_THREE,
};

type with_storage = enum u16 {
	ZERO,
	A_LOT = 0xCAFE,
};

export fn main() int = {
	assert(implicit::ZERO == 0);
	assert(implicit::ONE == 1);
	assert(implicit::TWO == 2);
	const val = implicit::TWO;
	assert(val == 2);

	assert(explicit::NEGATIVE == -1);
	assert(explicit::FORTY_TWO == 42);
	assert(explicit::FORTY_THREE == 43);
	const val = explicit::FORTY_TWO;
	assert(val == 42);

	assert(with_storage::ZERO == 0);
	assert(with_storage::A_LOT == 0xCAFE);
	const val = with_storage::A_LOT;
	assert(val == 0xCAFE);
	return 0;
};
