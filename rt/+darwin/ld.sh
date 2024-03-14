#!/usr/bin/env sh

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
