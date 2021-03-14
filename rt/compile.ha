// Runs the Hare compiler and returns the exit status.
export fn compile(src: const str) int = {
	let status = 0;
	let pipefd = [-1, -1];
	pipe2(&pipefd, 0);

	const child = fork();
	if (child == 0) {
		close(pipefd[1]);
		dup3(pipefd[0], 0, 0);
		close(1);
		close(2);

		const path = "./harec";
		const argv: [_]nullable *const char = [path, "-", null];
		const envp: [_]nullable *const char = [null];
		execve(path, &argv, &envp);
		abort();
	} else {
		close(pipefd[0]);

		const buf = src: *const char: *const [*]u8;
		for (let n = 0z; n < len(src)) {
			let m = write(pipefd[1], &buf[n], len(src) - n);
			assert(m > 0, "write(2) failed");
			n += m;
		};

		close(pipefd[1]);
		wait4(child, &status, 0, null);
	};

	return if (wifexited(status)) wexitstatus(status)
		else if (wifsignaled(status)) wtermsig(status)
		else abort();
};
