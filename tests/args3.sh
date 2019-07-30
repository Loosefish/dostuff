#!/usr/bin/env bash
# Exit on all errors and undefined vars
set -o errexit
set -o errtrace
set -o pipefail
set -o nounset

# call function with arg
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

cat > Dofile <<EOF
#!/bin/sh
do_one () {
	echo -n "\$1"
}
EOF

out=$(${ds} one "bar - baz" "quox")
ret=$?
if [ "$out" != "bar - baz" ]; then
    printf '%s\n' "$out"
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
