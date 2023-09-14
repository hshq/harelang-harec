#!/bin/sh

AS=/usr/bin/as

function fix_asm() {
sed -r \
    -e 's/^\.section "\.text([^"]*)"(,"([^"]*))?"$/.text # \1, \3/g' \
    -e 's/^\.section "\.data([^"]*)"$/.data # \1/g' \
    -e 's/^\.section "\.bss([^"]*)"$/.bss # \1/g' \
    \
    -e 's/^\.section "\.init_array"(,"([^"]*)")?$/.section __DATA, .init_array # \2/g' \
    -e 's/^\.section "\.fini_array"(,"([^"]*)")?$/.section __DATA, .fini_array # \2/g' \
    -e 's/^\.section "\.test_array"(,"([^"]*)")?$/.section __DATA, .test_array # \2/g' \
    \
    -e 's/^\.section \.abort "([^"]+)"$/.section __DATA, .abort # \1/g' \
    \
    -e 's/^(\.type .+)$/# \1/g' \
    -e 's/^(\.global) (rt\..+)$/\1 _\2/g' \
    -e 's/^(rt\..+:)$/_\1/g' \
    \
    $1
}

function header() {
    echo
    echo
    echo "# ----------------------------------------------------------------"
    echo ".file \"$1\""
    echo "# ----------------------------------------------------------------"
    echo
}


args=
code=
until [ $# -eq 0 ]; do
    case $1 in
        -o)
            args="$args $1 $2"
            shift
            ;;
        -g)
            args="$args $1"
            ;;
        -*)
            echo "Unsupported options: $*"
            exit 1
            ;;
        *)
            # file="$(fix_asm $1)"

            # # IFS=$'\n'
            file=$(cat "$1")

            code="$code$(header $1)\n$file"
            ;;
    esac
    shift
done

# echo "$code" | $AS $args --
echo "$code" | fix_asm | $AS $args --
