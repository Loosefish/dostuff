#!/bin/bash
# basic call of a named function
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"
touch Dofile

mkdir child
cd child
touch Dofile

mkdir child2
mkdir child2/child3
cd child2/child3
touch Dofile


${ds} -f > files
ret=$?

echo "${dir}/child/child2/child3/Dofile" > files.ok
echo "${dir}/child/Dofile" >> files.ok
echo "${dir}/Dofile" >> files.ok

diff files files.ok || ret=1

rm -rf "${dir}"
exit ${ret}
