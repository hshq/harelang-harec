#!/usr/bin/env sh

archs=('x86_64' 'aarch64')
comments=('#' ';')

arch="$(uname -m)"

# arch_config() {
    case "$1" in
        --arch)
            configs=(${archs[*]})
            ;;
        --comment)
            configs=(${comments[*]})
            ;;
        *)
            printf "Error: unsupported or unrecognized option %s\n" "$1"
            exit
            ;;
    esac
    case "$arch" in
        x86_64|amd64)
            echo ${configs[0]}
            ;;
        aarch64|arm64)
            echo ${configs[1]}
            ;;
        *)
            printf "Error: unsupported or unrecognized architecture %s\n" "$arch"
            exit
            ;;
    esac
# }
