#!/usr/bin/env sh

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
