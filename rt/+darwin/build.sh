#!/usr/bin/env sh


# -------------------------------- ARCH --------------------------------

function dict() {
    case "$(uname -m)" in
        x86_64|amd64)
            echo ${configs[0]}
            ;;
        aarch64|arm64)
            echo ${configs[1]}
            ;;
        *)
            printf "Error: unsupported or unrecognized architecture %s\n" "$(uname -m)"
            exit
            ;;
    esac
}

function ARCH() {
    configs=('x86_64' 'aarch64')
    dict
}

function ARCH_COMMENT() {
    configs=('#' ';')
    dict
}


# -------------------------------- QBE --------------------------------

function QBE() {
    # args=${*//amd64_sysv/amd64_apple}
    # qbe ${args//arm64/arm64_apple}

    args=${*/amd64_sysv/amd64_apple}
    qbe ${args/arm64/arm64_apple}
}


# -------------------------------- AS --------------------------------

COMMENT=$(ARCH_COMMENT)

function fix_asm() {
        # -e "s/^(\.section \.note\.GNU-stack,\"\",@progbits)$/$COMMENT \1/g" \
        # -e "s/^(\.section \.rodata)$/$COMMENT \1/g" \
        # -e "s/^(\.size .+)$/$COMMENT \1/g" \
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

function header() {
    echo
    echo
    echo "# ----------------------------------------------------------------"
    echo ".file \"$1\""
    echo "# ----------------------------------------------------------------"
    echo
}

function AS() {
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

    echo "$code" | fix_asm | as $args --
}


# -------------------------------- ld --------------------------------

function LD() {
    LDFLAGS="-e _start \
        -lSystem \
        -L$(xcrun --show-sdk-path -sdk macosx)/usr/lib"

    args=
    hasVal=
    for o in $@; do
        case $o in
            -T|-z)
                hasVal="$o"
                ;;
            --gc-sections|--script=?*)
                ;;
            *)
                if [ "$hasVal" != "" ]; then
                    hasVal=
                else
                    args="$args $o"
                fi
                ;;
        esac
    done

    ld $LDFLAGS $args
}


# -------------------------------- cc --------------------------------

function CC() {
    args=
    hasVal=
    for o in $@; do
        case $o in
            -T)
                hasVal="$o"
                ;;
            -Wl,--gc-sections)
                ;;
            -Wl,--no-gc-sections)
                ;;
            *)
                if [ "$hasVal" != "" ]; then
                    hasVal=
                else
                    args="$args $o"
                fi
                ;;
        esac
    done

    cc $args
}


# -------------------------------- main --------------------------------

STEP="$(basename $0)"
STEP=${STEP%.*}
STEP=${STEP##*-}
if [[ "$STEP" = "build" ]]; then
    STEP="$1"
    STEP=${STEP##*-}
    shift
fi
STEP=$(echo $STEP | tr '[:lower:]' '[:upper:]')

case "$STEP" in
    ARCH|QBE|AS|LD|CC)
        ;;
    *)
        printf "Error: unsupported or unrecognized option %s\n" "$STEP"
        echo $0 $@
        exit
        ;;
esac

eval $STEP $@
