export type status = enum {
	SUCCESS,
	USER,
	LEX,
	PARSE,
	CHECK,
	ABNORMAL = 255,
};

fn strstatus(status: int) str = {
	switch (status) {
	case status::SUCCESS =>
		return "success (0)";
	case status::USER =>
		return "user (1)";
	case status::LEX =>
		return "lex (2)";
	case status::PARSE =>
		return "parse (3)";
	case status::CHECK =>
		return "check (4)";
	case status::ABNORMAL =>
		return "abnormal (255)";
	case =>
		return itos(status: int);
	};
};

export type error = !void;

// Runs the Hare compiler and returns the exit status.
export fn compile(
	expected: (status | void),
	src: str,
	flags: str...
) (void | error) = {
	let wstatus = 0;
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
			let m = write(pipefd[1], &buf[n], len(src) - n): size;
			assert(m > 0, "write(2) failed");
			n += m;
		};

		close(pipefd[1]);
		wait4(child, &wstatus, 0, null);
	};

	if (!wifexited(wstatus)) {
		assert(wifsignaled(wstatus));
		let s = "signaled ";
		write(2, constchar(s), len(s));
		s = itos(wtermsig(wstatus));
		write(2, constchar(s), len(s));
		write(2, constchar("\n"), 1);
		return error;
	};

	match (expected) {
	case void => void;
	case let expected: status =>
		const status = wexitstatus(wstatus);
		if (status != expected) {
			let s = "expected ";
			write(2, constchar(s), len(s));
			s = strstatus(expected);
			write(2, constchar(s), len(s));
			s = ", got ";
			write(2, constchar(s), len(s));
			s = strstatus(status);
			write(2, constchar(s), len(s));
			write(2, constchar("\n"), 1);
			return error;
		};
	};
};
