# harec [![builds.sr.ht status](https://builds.sr.ht/~sircmpwn/harec/commits.svg)](https://builds.sr.ht/~sircmpwn/harec/commits?)

This is a [Hare](https://harelang.org) compiler written in C11 for
POSIX-compatible systems.

## Building

```
mkdir build
cd build
../configure
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
standard library](https://git.sr.ht/~sircmpwn/stdlib).
