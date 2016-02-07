#!/bin/sh
case $1 in
	-)
		grep "^do_.*().*{$" ./Dofile 2>/dev/null|\
			cut -f2- -d "_" |\
			cut -f1 -d "(" |\
			tr -d " "|\
			sed -e "s/^$/[default]/"
		;;
	*)
		if test -e ./Dofile; then
			source ./Dofile
			if grep "^do_$1.*().*{$" ./Dofile > /dev/null; then
				this=$1
				shift
				do_${this} "$@"
			else
				echo "dostuff: no such command \"$1\"" >&2
				exit 1
			fi
		else
			if test -d "$1"; then
				echo -e 'do_() {\n\techo "Do stuff!"\n}' > ./Dofile
				test -n "${EDITOR}" && ${EDITOR} ./Dofile
			else
				echo "dostuff: no Dofile" >&2
				exit 1
			fi
		fi
		;;
esac
