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
do_two () {
	echo -n "\$2"
}
EOF

out=$(${ds} two "bar - baz" "quox")
ret=$?
if [ "$out" != "quox" ]; then
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
