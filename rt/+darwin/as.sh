#!/bin/sh

arch=$(uname -m)
flags=-g

args=
path=
until [ $# -eq 1 ]; do
	args="$args $1"
	shift
done
path=$1

dir=".*\/rt\/\+$arch$"
if [[ $(dirname $path) =~ $dir ]]
then
	case $(basename -s .s $path) in
		cpuid_native|fenv|getfp|longjmp|restore|setjmp)
			sed -r \
    			-e 's/^(.section ".text.*")$/.text # \1/g' \
				-e 's/^([ \t]*)(.type[ \t]+.+)$/\1# \2/g' \
				-e 's/^([ \t]*.global[ \t]+)(rt\..+)$/\1_\2/g' \
				-e 's/^([ \t]*)(rt\..+:)$/\1_\2/g' \
				$path | /usr/bin/as $flags $args --
			exit 0
			;;
	esac
fi

/usr/bin/as $flags $args $path
exit 0
