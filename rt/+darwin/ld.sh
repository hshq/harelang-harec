#!/bin/sh

LDFLAGS="-e _start \
	-dead_strip \
	-lSystem \
	-L$(xcrun --show-sdk-path -sdk macosx)/usr/lib"

args=
hasVal=
for o in $*; do
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

/usr/bin/ld $LDFLAGS $args
