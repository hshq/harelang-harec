fn syscall0(_: u64) u64;
fn syscall1(_: u64, _: u64) u64;
fn syscall2(_: u64, _: u64, _: u64) u64;
fn syscall3(_: u64, _: u64, _: u64, _: u64) u64;
fn syscall4(_: u64, _: u64, _: u64, _: u64, _: u64) u64;
fn syscall5(_: u64, _: u64, _: u64, _: u64, _: u64, _: u64) u64;
fn syscall6(_: u64, _: u64, _: u64, _: u64, _: u64, _: u64, _: u64) u64;

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

export fn wifexited(status: int) bool = wtermsig(status) == 0;
export fn wexitstatus(status: int) int = (status & 0xff00) >> 8;

export fn wtermsig(status: int) int = status & 0x7f;
export fn wifsignaled(status: int) bool = (status & 0xffff) - 1 < 0xff;

export fn kill(pid: int, signal: int) int =
	syscall2(SYS_kill, pid: u64, signal: u64): int;

export fn pipe2(pipefd: *[2]int, flags: int) int =
	syscall2(SYS_pipe2, pipefd: uintptr: u64, flags: u64): int;

export def MAP_SHARED: uint		= 0x01;
export def MAP_PRIVATE: uint		= 0x02;
export def MAP_SHARED_VALIDATE: uint	= 0x03;
export def MAP_FIXED: uint		= 0x10;
export def MAP_ANON: uint		= 0x20;
export def MAP_NORESERVE: uint		= 0x4000;
export def MAP_GROWSDOWN: uint		= 0x0100;
export def MAP_DENYWRITE: uint		= 0x0800;
export def MAP_EXECUTABLE: uint		= 0x1000;
export def MAP_LOCKED: uint		= 0x2000;
export def MAP_POPULATE: uint		= 0x8000;
export def MAP_NONBLOCK: uint		= 0x10000;
export def MAP_STACK: uint		= 0x20000;
export def MAP_HUGETLB: uint		= 0x40000;
export def MAP_SYNC: uint		= 0x8000u;
export def MAP_FIXED_NOREPLACE: uint	= 0x100000;
export def MAP_FILE: uint		= 0;
export def MAP_HUGE_SHIFT: uint		= 26;
export def MAP_HUGE_MASK: uint		= 0x3f;
export def MAP_HUGE_64KB: uint		= 16 << 26;
export def MAP_HUGE_512KB: uint		= 19 << 26;
export def MAP_HUGE_1MB: uint		= 20 << 26;
export def MAP_HUGE_2MB: uint		= 21 << 26;
export def MAP_HUGE_8MB: uint		= 23 << 26;
export def MAP_HUGE_16MB: uint		= 24 << 26;
export def MAP_HUGE_32MB: uint		= 25 << 26;
export def MAP_HUGE_256MB: uint		= 28 << 26;
export def MAP_HUGE_512MB: uint		= 29 << 26;
export def MAP_HUGE_1GB: uint		= 30 << 26;
export def MAP_HUGE_2GB: uint		= 31 << 26;
export def MAP_HUGE_16GB: uint		= 34 << 26;

export def PROT_NONE: uint	= 0;
export def PROT_READ: uint 	= 1;
export def PROT_WRITE: uint	= 2;
export def PROT_EXEC: uint	= 4;
export def PROT_GROWSDOWN: uint	= 0x01000000;
export def PROT_GROWSUP: uint	= 0x02000000;

export fn mmap(
	addr: nullable *void,
	length: size,
	prot: uint,
	flags: uint,
	fd: int,
	offs: size
) *void = {
	let r = syscall6(SYS_mmap, addr: uintptr: u64, length: u64, prot: u64,
		flags: u64, fd: u64, offs: u64): u64;
	return if (r: int == -EPERM && addr == null
			&& flags & MAP_ANON > 0 && flags & MAP_FIXED == 0) {
		-ENOMEM: uintptr: *void; // Fix up incorrect EPERM from kernel
	} else r: uintptr: *void;
};

export fn munmap(addr: *void, length: size) int =
	syscall2(SYS_munmap, addr: uintptr: u64, length: u64): int;

export fn mprotect(addr: *void, length: size, prot: uint) int =
	syscall3(SYS_mprotect, addr: uintptr: u64, length: u64, prot: u64): int;

export def SIGABRT: int	= 6;
export def SIGCHLD: int	= 17;
