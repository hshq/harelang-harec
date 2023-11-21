#!/bin/sh

eval ". $HAREC_SRC/rt/+darwin/arch.sh"
arch="$(arch_config 'amd64_apple' 'arm64_apple')"

# qbe ${*//amd64_sysv/$arch}
qbe ${*/amd64_sysv/$arch}
