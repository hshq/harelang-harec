# harec

This is a [Hare](https://harelang.org) compiler written in C11 for
POSIX-compatible systems.

## Build status

<dl>
  <dt>Linux (x86_64)</dt><dd><a href="https://builds.sr.ht/~sircmpwn/harec/commits/master/alpine.yml"><img src="https://builds.sr.ht/~sircmpwn/harec/commits/master/alpine.yml.svg" alt="Build status for Linux" /></a></dd>
  <dt>FreeBSD (x86_64)</dt><dd><a href="https://builds.sr.ht/~sircmpwn/harec/commits/master/freebsd.yml"><img src="https://builds.sr.ht/~sircmpwn/harec/commits/master/freebsd.yml.svg" alt="Build status for FreeBSD" /></a></dd>
  <dt>Darwin (x86_64)</dt><dd><a href="https://github.com/hshq/harelang-harec/blob/master/.builds/darwin.yml"><img src="https://builds.sr.ht/~sircmpwn/harec/commits/master/darwin.yml.svg" alt="Build status for Darwin" /></a></dd>
  <dt>Darwin (aarch64)</dt><dd><a href="https://github.com/hshq/harelang-harec/blob/master/.builds/darwin.yml"><img src="https://builds.sr.ht/~sircmpwn/harec/commits/master/darwin.yml.svg" alt="Build status for Darwin" /></a></dd>
  <dt>NetBSD (x86_64)</dt><dd><a href="https://builds.sr.ht/~sircmpwn/harec/commits/master/netbsd.yml"><img src="https://builds.sr.ht/~sircmpwn/harec/commits/master/netbsd.yml.svg" alt="Build status for NetBSD" /></a></dd>
</dl>

## Building

```
cp configs/$platform.mk config.mk
make
```

Optionally, build and run the test suite as well:

```
make check
```

## Runtime

harec includes a minimal runtime under `rt` which is suitable for running the
test suite, but not recommended for production use. See `docs/runtime.txt` for
details on how to provide your own runtime implementation, or use the [Hare
standard library](https://git.sr.ht/~sircmpwn/hare).
