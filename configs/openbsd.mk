# install locations
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

# variables used during build
PLATFORM = openbsd
ARCH = x86_64
HARECFLAGS =
QBEFLAGS =
ASFLAGS =
LDLINKFLAGS = -z nobtcfi
CFLAGS = -g -std=c11 -D_XOPEN_SOURCE=700 -Iinclude \
	-Wall -Wextra -Werror -pedantic -Wno-unused-parameter
LDFLAGS =
LIBS = -lm

# commands used by the build script
CC = cc
# OpenBSD: gas is in the binutils package. as from the base system is too old.
AS = gas
LD = ld
QBE = qbe

# build locations
HARECACHE = .cache
BINOUT = .bin

# variables that will be embedded in the binary with -D definitions
DEFAULT_TARGET = $(ARCH)
VERSION = $$(./scripts/version)
