// Allocates a segment.
fn segmalloc(n: size) nullable *opaque = {
	let p: *opaque = mmap(null, n,
		PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANON, -1, 0);
	return if (p == MAP_FAILED) null else p;
};

def MAP_FAILED: *void = -1: uintptr: *void;

// Frees a segment allocated with segmalloc.
fn segfree(p: *opaque, s: size) int = munmap(p, s);
