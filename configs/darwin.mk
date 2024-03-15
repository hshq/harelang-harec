# install locations
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

# variables used during build
PLATFORM = darwin
# ARCH = $(shell source "./rt/+darwin/arch.sh" arch)
# ARCH = $(shell hare-build.sh arch)
ARCH = $(shell hare-arch.sh)
HARECFLAGS =
QBEFLAGS =
ASFLAGS =
# LDLINKFLAGS = --gc-sections -z noexecstack
CFLAGS = -g -std=c11 -D_XOPEN_SOURCE=700 -Iinclude \
	-Wall -Wextra -Werror -pedantic -Wno-unused-parameter
LDFLAGS =
LIBS = -lm

# commands used by the build script
# CC = hare-cc.sh
CC = cc
# AS = hare-build.sh as
AS = hare-as.sh
# LD = hare-build.sh -ld
LD =  hare-ld.sh
# QBE = hare-qbe.sh
# QBE = hare-build.sh --qbe
QBE = qbe

# build locations
HARECACHE ?= .cache
BINOUT = .bin

# variables that will be embedded in the binary with -D definitions
DEFAULT_TARGET = $(ARCH)
VERSION = $$(./scripts/version)
