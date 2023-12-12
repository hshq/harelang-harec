export type exited = int, signaled = int;
export type exit_status = (exited | signaled);

// Runs the Hare compiler and returns the exit status.
export fn compile(src: const str, flags: str...) exit_status = {
	let status = 0;
	let pipefd = [-1, -1];
	assert(pipe2(&pipefd, 0) == 0);

	const child = fork();
	if (child == 0) {
		close(pipefd[1]);
		dup2(pipefd[0], 0);
		close(1);
		close(2);

		let argv: []nullable *const u8 = [];
		defer for (let i = 0z; i < len(argv); i += 1) {
			free(argv[i]);
		};
		// FIXME use $BINOUT variable
		append(argv, alloc_constchar("./.bin/harec"));
		for (let i = 0z; i < len(flags); i += 1) {
			append(argv, alloc_constchar(flags[i]));
		};
		append(argv, alloc_constchar("-o/dev/null"));
		append(argv, alloc_constchar("-"));
		append(argv, null);

		execve(constchar("./.bin/harec\0"), *(&argv: **[*]nullable *const u8), envp);
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

	return if (wifexited(status)) wexitstatus(status): exited
		else if (wifsignaled(status)) wtermsig(status): signaled
		else abort();
};
