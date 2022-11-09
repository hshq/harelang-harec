#!/bin/sh

if [ "$1" == "-h" ] || [ "`uname -s`" != "Darwin" ];
then
    qbe $*
    exit
fi

qbe -G m $*

OUT=
while getopts 'ho:t:G:d:' OPT; do
    case $OPT in
        o) OUT="$OPTARG";;
    esac
done

sed -r -I '' \
    -e 's/^(.section ".text.*")$/.text # \1/g' \
    -e 's/^(.section ".data.*")$/.data # \1/g' \
    -e 's/^(.section ".bss.*")$/.data # \1/g' \
    -e 's/^.section ".init_array"$/.section __DATA, .init_array/g' \
    -e 's/^.section ".fini_array"$/.section __DATA, .fini_array/g' \
    -e 's/^.section ".test_array"$/.section __DATA, .test_array/g' \
    -e 's/^.section ".init_array", (.+)$/.section __DATA, .init_array # \1/g' \
    -e 's/^.section ".fini_array", (.+)$/.section __DATA, .fini_array # \1/g' \
    -e 's/^.section ".test_array", (.+)$/.section __DATA, .test_array # \1/g' \
    $OUT
