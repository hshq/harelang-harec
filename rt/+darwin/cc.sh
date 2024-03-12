#!/usr/bin/env sh

args=
hasVal=
for o in $*; do
	case $o in
		-T)
			hasVal="$o"
            args="$args $HAREC_SRC/rt/+darwin/start+libc.s"
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

/usr/bin/cc $args
