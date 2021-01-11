// Runs the Hare compiler and returns the exit status.
export fn compile(src: str) int = {
	let status = 0;
	let pipefd = [-1, -1];
	pipe2(&pipefd, 0);

	const child = fork();
	if (child == 0) {
		close(pipefd[1]);
		dup2(pipefd[0], 0);
		close(1);
		close(2);

		const path = "./harec";
		const argv: [3]nullable *const char = [path, "-", null];
		const envp: [1]nullable *const char = [null];
		execve(path, &argv, &envp);
		abort();
	} else {
		close(pipefd[0]);

		let buf = src: *const char: *const [*]u8;
		for (let n = 0z; n < len(src)) {
			let m = write(pipefd[1], &buf[n], len(src) - n);
			assert(m: i64 > 0i64, "write(2) failed");
			n += m;
		};

		close(pipefd[1]);
		wait4(child, &status, 0, null);
	};

	return wexitstatus(status);
};
