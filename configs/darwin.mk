# install locations
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

# variables used during build
PLATFORM = darwin
ARCH = $(shell source "$(HAREC_SRC)/rt/+darwin/arch.sh" --arch)
HARECFLAGS =
QBEFLAGS =
ASFLAGS =
# LDLINKFLAGS = --gc-sections -z noexecstack
LDLINKFLAGS =
CFLAGS = -g -std=c11 -D_XOPEN_SOURCE=700 -Iinclude \
	-Wall -Wextra -Werror -pedantic -Wno-unused-parameter
LDFLAGS =
LIBS = -lm

# commands used by the build script
CC = /usr/bin/cc
AS =  ./rt/+darwin/as.sh
LD =  ./rt/+darwin/ld.sh
QBE = ./rt/+darwin/qbe.sh

# build locations
# HARECACHE = .cache
HARECACHE = /Volumes/hare-cache/harec
BINOUT = .bin

# variables that will be embedded in the binary with -D definitions
DEFAULT_TARGET = $(ARCH)
VERSION = $$(./scripts/version)
