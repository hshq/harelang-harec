#!/bin/sh

eval ". $HAREC_SRC/rt/+darwin/arch.sh" "amd64_apple" "arm64_apple"
arch="$(arch_config '#' ';')"

# qbe ${*//amd64_sysv/$arch}
qbe ${*/amd64_sysv/$arch}
