// Runs the Hare compiler and returns the exit status.
export fn compile(src: const str) int = {
	let status = 0;
	let pipefd = [-1, -1];
	pipe2(&pipefd, 0);

	const child = fork();
	if (child == 0) {
		close(pipefd[1]);
		dup2(pipefd[0], 0);
		close(1);
		close(2);

		const path = "./harec\0";
		const argv: [_]nullable *const char = [
			constchar(path),
			constchar("-\0"),
			null
		];
		const envp: [_]nullable *const char = [
			constchar("HARECACHE=mod\0"),
			null
		];
		execve(constchar(path), &argv, &envp);
		abort();
	} else {
		assert(child != -1, "fork(2) failed");
		close(pipefd[0]);

		const buf = constchar(src): *const [*]u8;
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
