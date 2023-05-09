export type exited = int, signaled = int;
export type exit_status = (exited | signaled);

export type typedef = str;
export type define = str;
export type compileflag = (typedef | define);

// Runs the Hare compiler and returns the exit status.
export fn compile(src: const str, flags: compileflag...) exit_status = {
	let status = 0;
	let pipefd = [-1, -1];
	assert(pipe2(&pipefd, 0) == 0);

	const child = fork();
	if (child == 0) {
		close(pipefd[1]);
		dup2(pipefd[0], 0);
		close(1);
		close(2);

		const path = "./harec\0";
		let argv: []nullable *const u8 = [];
		append(argv, constchar(path));
		for (let i = 0z; i < len(flags); i += 1) {
			match (flags[i]) {
			case let d: define =>
				append(argv, constchar("-D\0"));
				append(argv, alloc_constchar(d));
			case let t: typedef =>
				append(argv, constchar("-t\0"));
				append(argv, alloc_constchar(t));
			};
		};
		append(argv, constchar("-\0"));
		append(argv, null);

		const envp: [_]nullable *const u8 = [
			constchar("HARECACHE=mod\0"),
			null
		];
		execve(constchar(path), *(&argv: **[*]nullable *const u8), &envp);
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
