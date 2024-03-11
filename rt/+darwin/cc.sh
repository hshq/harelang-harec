#!/usr/bin/env sh

CCFLAGS=
args=
hasVal=
for o in $*; do
	case $o in
		-T)
			hasVal="$o"
			;;
        --gc-sections)
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

/usr/bin/cc $CCFLAGS $args
