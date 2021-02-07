fn syscall0(u64) u64;
fn syscall1(u64, u64) u64;
fn syscall2(u64, u64, u64) u64;
fn syscall3(u64, u64, u64, u64) u64;
fn syscall4(u64, u64, u64, u64, u64) u64;
fn syscall5(u64, u64, u64, u64, u64, u64) u64;
fn syscall6(u64, u64, u64, u64, u64, u64, u64) u64;

export fn write(fd: int, buf: *const void, count: size) size =
	syscall3(SYS_write, fd: u64, buf: uintptr: u64, count: u64): size;

export fn close(fd: int) int = syscall1(SYS_close, fd: u64): int;

export fn dup3(old: int, new: int, flags: int) int =
	syscall3(SYS_dup3, old: u64, new: u64, flags: u64): int;

export fn getpid() int = syscall0(SYS_getpid): int;

export @noreturn fn exit(status: int) void = syscall1(SYS_exit, status: u64);

export fn fork() int = syscall2(SYS_clone, SIGCHLD: u64, 0u64): int;

export fn execve(
	path: *const char,
	argv: *[*]nullable *const char,
	envp: *[*]nullable *const char,
) int = syscall3(SYS_execve,
	path: uintptr: u64,
	argv: uintptr: u64,
	envp: uintptr: u64): int;

export fn wait4(pid: int, status: *int, options: int, rusage: nullable *void) void =
	syscall4(SYS_wait4, pid: u64, status: uintptr: u64,
		options: u64, rusage: uintptr: u64);

export fn wexitstatus(status: int) int = status & 0xff00 >> 8;

export fn kill(pid: int, signal: int) int =
	syscall2(SYS_kill, pid: u64, signal: u64): int;

export fn pipe2(pipefd: *[2]int, flags: int) int =
	syscall2(SYS_pipe2, pipefd: uintptr: u64, flags: u64): int;

export def MAP_SHARED: uint		= 0x01u;
export def MAP_PRIVATE: uint		= 0x02u;
export def MAP_SHARED_VALIDATE: uint	= 0x03u;
export def MAP_FIXED: uint		= 0x10u;
export def MAP_ANON: uint		= 0x20u;
export def MAP_NORESERVE: uint		= 0x4000u;
export def MAP_GROWSDOWN: uint		= 0x0100u;
export def MAP_DENYWRITE: uint		= 0x0800u;
export def MAP_EXECUTABLE: uint		= 0x1000u;
export def MAP_LOCKED: uint		= 0x2000u;
export def MAP_POPULATE: uint		= 0x8000u;
export def MAP_NONBLOCK: uint		= 0x10000u;
export def MAP_STACK: uint		= 0x20000u;
export def MAP_HUGETLB: uint		= 0x40000u;
export def MAP_SYNC: uint		= 0x80000u;
export def MAP_FIXED_NOREPLACE: uint	= 0x100000u;
export def MAP_FILE: uint		= 0u;
export def MAP_HUGE_SHIFT: uint		= 26u;
export def MAP_HUGE_MASK: uint		= 0x3fu;
export def MAP_HUGE_64KB: uint		= 16u << 26u;
export def MAP_HUGE_512KB: uint		= 19u << 26u;
export def MAP_HUGE_1MB: uint		= 20u << 26u;
export def MAP_HUGE_2MB: uint		= 21u << 26u;
export def MAP_HUGE_8MB: uint		= 23u << 26u;
export def MAP_HUGE_16MB: uint		= 24u << 26u;
export def MAP_HUGE_32MB: uint		= 25u << 26u;
export def MAP_HUGE_256MB: uint		= 28u << 26u;
export def MAP_HUGE_512MB: uint		= 29u << 26u;
export def MAP_HUGE_1GB: uint		= 30u << 26u;
export def MAP_HUGE_2GB: uint		= 31u << 26u;
export def MAP_HUGE_16GB: uint		= 34u << 26u;

export def PROT_NONE: uint	= 0u;
export def PROT_READ: uint 	= 1u;
export def PROT_WRITE: uint	= 2u;
export def PROT_EXEC: uint	= 4u;
export def PROT_GROWSDOWN: uint	= 0x01000000u;
export def PROT_GROWSUP: uint	= 0x02000000u;

export fn mmap(
	addr: nullable *void,
	length: size,
	prot: uint,
	flags: uint,
	fd: int,
	offs: size
) *void = {
	let r: u64 = syscall6(SYS_mmap, addr: uintptr: u64,
		length: u64, prot: u64, flags: u64, fd: u64, offs: u64): u64;
	// TODO: Type promotion
	return if (r: int == -EPERM && addr: uintptr == null: uintptr
			&& (flags & MAP_ANON) > 0u && (flags & MAP_FIXED) == 0u) {
		(-ENOMEM): uintptr: *void; // Fix up incorrect EPERM from kernel
	} else r: uintptr: *void;
};

export fn munmap(addr: *void, length: size) int =
	syscall2(SYS_munmap, addr: uintptr: u64, length: u64): int;

export fn mprotect(addr: *void, length: size, prot: uint) int =
	syscall3(SYS_mprotect, addr: uintptr: u64, length: u64, prot: u64): int;

export def SIGABRT: int	= 6;
export def SIGCHLD: int	= 17;
