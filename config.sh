outdir=${OUTDIR:-.build}
srcdir=${SRCDIR:-$(dirname "$0")}
AR=${AR:-ar}
AS=${AS:-as}
CC=${CC:-cc}
CFLAGS=${CFLAGS:-}
DEFAULT_TARGET=
LDFLAGS=${LDFLAGS:-}
LD=${LD:-ld}
QBE=${QBE:-qbe}

if [ `uname -s` = "OpenBSD" ]
then
	LD="${LD:-ld} -nopie"
fi

checkprog=/dev/null

if [ `uname -s` = "Darwin" ]
then
	checkprog="$outdir"/check
fi

for arg
do
	case "$arg" in
		--bindir=*)
			BINDIR=${arg#*=}
			;;
		--libdir=*)
			LIBDIR=${arg#*=}
			;;
		--mandir=*)
			MANDIR=${arg#*=}
			;;
		--prefix=*)
			PREFIX=${arg#*=}
			if [ "$PREFIX" = "/usr" ]
			then
				SYSCONFDIR=/etc
			fi
			;;
		--sharedir=*)
			SHAREDIR=${arg#*=}
			;;
		--sysconfdir=*)
			SYSCONFDIR=${arg#*=}
			;;
		--target=*)
			DEFAULT_TARGET=${arg#*=}
			;;
		--help)
			cat >&2 <<'EOF'
Usage: configure [options...]

Options:
--help
--prefix=<path>
	default: /usr/local
--bindir=<path>
	default: (prefix)/bin
--libdir=<path>
	default: (prefix)/lib
--mandir=<path>
	default: (prefix)/share/man
--sharedir=<path>
	default: (prefix)/share
--sysconfdir=<path>
	default: /etc if prefix is /usr, otherwise (prefix)/etc
--target=<arch>
	default: (current arch)

Environment variables:
AR	(default: "ar")
AS	(default: "as")
CC	(default: "cc")
CFLAGS	(default: "")
LD	(default: "ld")
LDFLAGS	(default: "")
OUTDIR	(default: ".build")
QBE	(default: "qbe")
SRCDIR	(default: directory of configure script)
EOF
			exit
			;;
		*)
			printf 'Error: unknown configure option %s\n' "$arg" >&2
			exit 1
			;;
	esac
done

subdir() {
	eval ". $srcdir/$1/configure"
}

headers() {
	printf 'HEADERS=\\\n'
	while [ $# -ne 0 ]
	do
		printf '\tinclude/%s \\\n' "$1"
		shift
	done
}

genrules() {
	target="$1"
	shift
	printf '# Begin generated rules for %s\n' "$target"
	for file in "$@"
	do
		ext="${file#*.}"
		file="${file%.*}"
		deps=
		if [ $ext = "ha" ]
		then
			deps=" harec"
		fi
		printf '%s.o: %s.%s%s $(HEADERS)\n' "$file" "$file" "$ext" "$deps"
	done
	printf '%s_objects=\\\n' "$target"
	n=0
	for file in "$@"
	do
		file="${file%.*}"
		n=$((n+1))
		if [ $n -eq $# ]
		then
			printf '\t%s.o\n' "$file"
		else
			printf '\t%s.o \\\n' "$file"
		fi
	done
	printf '# End generated rules for %s\n' "$target"
}

append_cflags() {
	for flag
	do
		CFLAGS="$(printf '%s \\\n\t%s' "$CFLAGS" "$flag")"
	done
}

test_cflags() {
	[ ! -e "$outdir"/check.c ] && cat <<-EOF > "$outdir"/check.c
	int main(void) { return 0; }
	EOF
	werror=""
	case "$CFLAGS" in
		*-Werror*)
			werror="-Werror"
			;;
	esac
	if $CC $werror "$@" -o $checkprog "$outdir"/check.c >/dev/null 2>&1
	then
		append_cflags "$@"
	else
		return 1
	fi
}

find_library() {
	name="$1"
	pc="$2"
	printf "Checking for %s... " "$name"
	if ! pkg-config "$pc" 2>/dev/null
	then
		printf "NOT FOUND\n"
		printf "Tried pkg-config %s\n" "$pc"
		return 1
	fi
	printf "OK\n"
	CFLAGS="$CFLAGS $(pkg-config --cflags "$pc")"
	LIBS="$LIBS $(pkg-config --libs "$pc")"
}

run_configure() {
	mkdir -p "$outdir"

	for flag in -g -std=c11 -D_XOPEN_SOURCE=700 \
		-Wall -Wextra -Werror -pedantic \
		-Wno-unused-parameter
	do
		printf "Checking for %s... " "$flag"
		if test_cflags "$flag"
		then
			echo yes
		else
			echo no
		fi
	done

	if [ -z "$DEFAULT_TARGET" ]
	then
		printf "Detecting machine architecture... "
		arch="$(uname -m)"
		case "$arch" in
			x86_64|amd64)
				DEFAULT_TARGET=x86_64
				;;
			aarch64|arm64)
				DEFAULT_TARGET=aarch64
				;;
			riscv64)
				DEFAULT_TARGET=riscv64
				;;
			*)
				printf "Error: unsupported or unrecognized architecture %s\n" "$arch"
				;;
		esac
		printf '%s\n' "$DEFAULT_TARGET"
	fi
	append_cflags -DDEFAULT_TARGET="\\\"$DEFAULT_TARGET\\\""

	printf "Checking for qbe... "
	if $QBE -h > /dev/null 2>&1
	then
		echo yes
	else
		echo no
		echo "Error: no qbe binary found"
		exit 1
	fi

	printf "Creating %s/config.mk... " "$outdir"
	cat <<EOF > "$outdir"/config.mk
CC=$CC
AS=$AS
LD=$LD
AR=$AR
QBE=$QBE
LIBS=$LIBS -lm
PREFIX=${PREFIX:-/usr/local}
OUTDIR=${outdir}
SRCDIR=${srcdir}
BINDIR=${BINDIR:-\$(PREFIX)/bin}
SHAREDIR=${SHAREDIR:-\$(PREFIX)/share}
SYSCONFDIR=${SYSCONFDIR:-\$(PREFIX)/etc}
LIBDIR=${LIBDIR:-\$(PREFIX)/lib}
MANDIR=${MANDIR:-\$(PREFIX)/share/man}
VARLIBDIR=${MANDIR:-\$(PREFIX)/var/lib}
CACHE=\$(OUTDIR)/cache
CFLAGS=${CFLAGS} \\
	-Iinclude -I\$(OUTDIR) \\
	-DPREFIX='"\$(PREFIX)"' \\
	-DLIBDIR='"\$(LIBDIR)"' \\
	-DVARLIBDIR='"\$(VARLIBDIR)"' \\
	-DSYSCONFDIR='"\$(SYSCONFDIR)"' \\
	-DVERSION='"'"\$\$(./scripts/version)"'"'
LDFLAGS=${LDFLAGS}
HARECACHE=./mod

all: ${all}
EOF

	for target in $all
	do
		$target >>"$outdir"/config.mk
	done
	echo done

	cat <<-EOF >harec.sh
	export PATH="$(pwd)":\$PATH
	EOF

	if [ "$srcdir" = "." ]
	then
		return
	fi

	populate() (
		path="$1"
		mkdir -p "${path#$srcdir/}"
		fullpath() ( cd "$1" && pwd )
		for orig in "$path"/*
		do
			link="${orig#$srcdir/}"
			if [ -d "$orig" ]
			then
				mkdir -p $link
				populate "$orig"
			elif [ -f "$orig" ]
			then
				ln -sf "$(fullpath "$path")"/"$(basename "$orig")" "$link"
			fi
		done
	)

	printf "Populating build dir... "
	populate "$srcdir/doc"
	populate "$srcdir/include"
	populate "$srcdir/rt"
	populate "$srcdir/scripts"
	populate "$srcdir/src"
	populate "$srcdir/tests"
	populate "$srcdir/testmod"
	ln -sf "$srcdir"/Makefile ./
	echo done
}
