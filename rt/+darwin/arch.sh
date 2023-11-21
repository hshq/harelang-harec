#!/bin/sh

arch="`uname -m`"

arch_config() {
    case "$arch" in
        x86_64|amd64)
            echo $1
            ;;
        arm64)
            echo $2
            ;;
        *)
            printf "Error: unsupported or unrecognized architecture %s\n" "$M"
            exit
            ;;
    esac
}
