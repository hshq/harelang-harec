@symbol("main") fn main() int;
fn exit(status: int) void;

export fn start_ha(iv: [*]uintptr) void = {
	main();
	exit(0);
};
