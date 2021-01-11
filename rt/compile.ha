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
		const param = "-";
		const argv: [3]nullable *const char = [path, param, null];
		const envp: [1]nullable *const char = [null];
		execve(path: *const char, &argv, &envp);
		abort();
	} else {
		close(pipefd[0]);
		write(pipefd[1], src: *const char, len(src));
		close(pipefd[1]);
		wait4(child, &status, 0, null);
	};

	return wexitstatus(status);
};
