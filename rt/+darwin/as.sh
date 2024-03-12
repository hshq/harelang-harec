#!/bin/sh

# eval ". $HAREC_SRC/rt/+darwin/arch.sh"
# COMMENT="$(arch_config --comment)"
COMMENT=$(source "$HAREC_SRC/rt/+darwin/arch.sh" --comment)
ARCH=$(source "$HAREC_SRC/rt/+darwin/arch.sh" --arch)

AS="/usr/bin/as -I$HAREC_SRC/rt/+darwin"

function fix_asm() {
    # -e 's/^(\.section \.note\.GNU-stack,\"\",@progbits)$/$COMMENT \1/g' \
    # -e 's/^(\.section \.rodata)$/$COMMENT \1/g' \
    # -e 's/^(\.size .+)$/$COMMENT \1/g' \
    sed \
        -e "s/^\.section \"\.text([^\"]*)\"(,\"([^\"]*))?\"$/.text $COMMENT \1, \3/g" \
        -e "s/^\.section \"\.data([^\"]*)\"$/.data $COMMENT \1/g" \
        -e "s/^\.section \"\.bss([^\"]*)\"$/.bss $COMMENT \1/g" \
        \
        -e "s/^\.section \"\.init_array\"(,\"([^\"]*)\")?$/.section __DATA, .init_array $COMMENT \2/g" \
        -e "s/^\.section \"\.fini_array\"(,\"([^\"]*)\")?$/.section __DATA, .fini_array $COMMENT \2/g" \
        -e "s/^\.section \"\.test_array\"(,\"([^\"]*)\")?$/.section __DATA, .test_array $COMMENT \2/g" \
        \
        -e "s/^\.section \.abort \"([^\"]+)\"$/.section __DATA, .abort $COMMENT \1/g" \
        \
        -e "s/^(\.type .+)$/$COMMENT \1/g" \
        -e "s/^(\.global) (rt\..+)$/\1 _\2/g" \
        -e "s/^(rt\..+:)$/_\1/g" \
        -e "s/^(crypto\.aes\.x86ni_.+:)$/_\1/g" \
        -e "s/^(debug\.getfp:)$/_\1/g" \
        \
        -e "s/adrp	x0, _environ@page/adrp	x0, _environ@GOTPAGE/g" \
        -e "s/add	x0, x0, _environ@pageoff/ldr	x0, [x0, _environ@GOTPAGEOFF]/g" \
        \
        -r
}

function fix_got() {
    if [[ "$ARCH" != "aarch64" ]]; then
        cat -
        return
    fi
    sed \
        -e "s/adrp	x0, _environ@page/adrp	x0, _environ@GOTPAGE/g" \
        -e "s/add	x0, x0, _environ@pageoff/ldr	x0, [x0, _environ@GOTPAGEOFF]/g" \
        \
        -r
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
echo "$code" | fix_asm | fix_got | $AS $args --
