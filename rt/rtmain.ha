@symbol("main") fn main() void;

const @symbol("__init_array_start") init_start: [*]*fn() void;
const @symbol("__init_array_end") init_end: [*]*fn() void;
const @symbol("__fini_array_start") fini_start: [*]*fn() void;
const @symbol("__fini_array_end") fini_end: [*]*fn() void;

let argc: size = 0;
let argv: *[*]*const u8 = null: *[*]*const u8;
let envp: *[*]nullable *const u8 = null: *[*]nullable *const u8;

export let status = 0;

export @noreturn fn start_ha(iv: *[*]uintptr) void = {
	const ninit = (&init_end: uintptr - &init_start: uintptr): size
		/ size(*fn() void);
	for (let i = 0z; i < ninit; i += 1) {
		init_start[i]();
	};

	argc = iv[0]: size;
	argv = &iv[1]: *[*]*const u8;
	envp = &argv[argc + 1]: *[*]nullable *const u8;

	main();

	const nfini = (&fini_end: uintptr - &fini_start: uintptr): size
		/ size(*fn() void);
	for (let i = 0z; i < nfini; i += 1) {
		fini_start[i]();
	};

	exit(status);
};
