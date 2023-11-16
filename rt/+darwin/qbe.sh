#!/bin/sh

M="`uname -m`"

case "$M" in
    x86_64|amd64)
        M="amd64_apple"
        ;;
    arm64)
        M="arm64_apple"
        ;;
    *)
        printf "Error: unsupported or unrecognized architecture %s\n" "$M"
        exit
        ;;
esac

# qbe ${*//amd64_sysv/$M}
qbe ${*/amd64_sysv/$M}
