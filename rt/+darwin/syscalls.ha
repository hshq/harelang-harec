fn syscall0(u64) u64;
fn syscall1(u64, u64) u64;
fn syscall2(u64, u64, u64) u64;
fn syscall3(u64, u64, u64, u64) u64;
fn syscall4(u64, u64, u64, u64, u64) u64;
fn syscall5(u64, u64, u64, u64, u64, u64) u64;
fn syscall6(u64, u64, u64, u64, u64, u64, u64) u64;

fn sys_pipe(u64, u64) u64;
fn sys_fork(u64) u64;

export fn write(fd: int, buf: *const opaque, count: size) size =
	syscall3(SYS_write, fd: u64, buf: uintptr: u64, count: u64): size;

// export fn close(fd: int) int = syscall1(SYS_close, fd: u64): int;
export fn close(fd: int) void = syscall1(SYS_close, fd: u64): void;

export fn dup2(old: int, new: int) int =
	syscall2(SYS_dup2,  old: u64, new: u64): int;

export fn getpid() int = syscall0(SYS_getpid): int;

export def EXIT_SUCCESS: int = 0;

export fn exit(status: int) never = {
	syscall1(SYS_exit, status: u64);
	abort();
};

export fn fork() int = sys_fork(SYS_fork): int;
// export fn fork() int = {
// 	let child = syscall0(SYS_fork): int;
// 	let self = getpid();
// 	return if (self == child) 0 else child;
// };

export fn execve(
	path: *const u8,
	argv: *[*]nullable *const u8,
	envp: *[*]nullable *const u8,
) int = syscall3(SYS_execve,
	path: uintptr: u64,
	argv: uintptr: u64,
	envp: uintptr: u64): int;

export fn wait4(pid: int, status: *int, options: int, rusage: nullable *opaque) void = {
	syscall4(SYS_wait4, pid: u64, status: uintptr: u64,
		options: u64, rusage: uintptr: u64);
};

// <sys/wait.h>: WIFEXITED, WEXITSTATUS, WTERMSIG(_WSTATUS), WIFSIGNALED
export fn wifexited(status: int) bool = wtermsig(status) == 0;
export fn wexitstatus(status: int) int = (status & 0xff00) >> 8;
// export fn wexitstatus(status: int) int = (status >> 8) & 0x00ff;

export fn wtermsig(status: int) int = status & 0x7f;
export fn wifsignaled(status: int) bool =
	// wtermsig(status) != 0o177 && wtermsig(status) != 0 && status != 0x13;
	wtermsig(status) != 0o177 && wtermsig(status) != 0;

export fn kill(pid: int, signal: int) int =
	syscall2(SYS_kill, pid: u64, signal: u64): int;

export fn pipe2(pipefd: *[2]int, flags: int) int =
	// syscall2(SYS_pipe2, pipefd: uintptr: u64, flags: u64): int;
	sys_pipe(SYS_pipe, pipefd: uintptr: u64): int;

export def MAP_SHARED:        uint = 0x0001;
export def MAP_PRIVATE:       uint = 0x0002;
export def MAP_FIXED:         uint = 0x0010;
export def MAP_HASSEMAPHORE:  uint = 0x0200;
export def MAP_FILE:          uint = 0x0000;
export def MAP_ANON:          uint = 0x1000;

def PROT_NONE:  uint = 0x00;
def PROT_READ:  uint = 0x01;
def PROT_WRITE: uint = 0x02;
def PROT_EXEC:  uint = 0x04;

export fn mmap(
	addr: nullable *opaque,
	length: size,
	prot: uint,
	flags: uint,
	fd: int,
	offs: size
) *opaque = {
	return syscall6(SYS_mmap, addr: uintptr: u64, length: u64, prot: u64,
		flags: u64, fd: u64, offs: u64): uintptr: *opaque;
};

export fn munmap(addr: *opaque, length: size) int =
	syscall2(SYS_munmap, addr: uintptr: u64, length: u64): int;

export def SIGABRT: int	= 6;
export def SIGCHLD: int	= 20;
