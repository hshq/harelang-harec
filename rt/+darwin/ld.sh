#!/bin/sh

LDFLAGS="-e _start \
	-lSystem \
	-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"

args=
hasVal=
for o in $*; do
	case $o in
		-T)
			hasVal="$o"
			;;
		--gc-sections|--script=?*|-T?*)
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

/usr/bin/ld $LDFLAGS $args
