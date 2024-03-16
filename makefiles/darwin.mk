RTSCRIPT = rt/hare+$(PLATFORM).sc

_rt_ha = \
	rt/malloc.ha \
	rt/+$(PLATFORM)/syscallno.ha \
	rt/+$(PLATFORM)/segmalloc.ha

_rt_s = \
	rt/+$(PLATFORM)/start+$(ARCH).s \
	rt/+$(PLATFORM)/@func-libc.s \
	rt/+$(PLATFORM)/syscall+$(ARCH).s
