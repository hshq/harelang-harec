RTSCRIPT = rt/hare.sc

_rt_ha = \
	rt/malloc.ha \
	rt/+$(PLATFORM)/syscallno.ha \
	rt/+$(PLATFORM)/segmalloc.ha

_rt_s = \
	rt/+$(PLATFORM)/start+$(ARCH).s \
	rt/+$(PLATFORM)/syscall+$(ARCH).s
