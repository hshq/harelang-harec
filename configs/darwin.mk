# install locations
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

# variables used during build
PLATFORM = darwin
ARCH = $(shell hare-arch.sh --arch)
HARECFLAGS =
QBEFLAGS =
ASFLAGS =
# LDLINKFLAGS = --gc-sections -z noexecstack
LDLINKFLAGS = -dead_strip
CFLAGS = -g -std=c11 -D_XOPEN_SOURCE=700 -Iinclude \
	-Wall -Wextra -Werror -pedantic -Wno-unused-parameter
LDFLAGS = -dead_strip
LIBS = -lm

# commands used by the build script
CC = cc
AS = hare-as.sh
LD =  hare-ld.sh
QBE = qbe

# build locations
# HARECACHE = .cache
HARECACHE = /Volumes/hare-cache/harec.cache
BINOUT = .bin

# variables that will be embedded in the binary with -D definitions
DEFAULT_TARGET = $(ARCH)
VERSION = $$(./scripts/version)
