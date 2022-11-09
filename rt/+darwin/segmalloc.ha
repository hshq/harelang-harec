// Allocates a segment.
fn segmalloc(n: size) nullable *void = {
	let p: *void = mmap(null, n,
		PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANON, -1, 0);
	return if (p == MAP_FAILED) null else p;
};

def MAP_FAILED: *void = -1: uintptr: *void;

// Frees a segment allocated with segmalloc.
fn segfree(p: *void, s: size) int = munmap(p, s);
