#!/bin/bash
# call function with same name in a child dir
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

cat > Dofile << EOF
#!/bin/sh
do_foo () {
	echo -n "6446"
}
do_gronk () {
	echo -n "6446"
}
do_bar () {
	echo -n "6447"
}
EOF

mkdir child
cd child

cat > Dofile << EOF
#!/bin/sh
do_(){
	ls
}
do_bar () {
	echo -n "6447"
}
do_baz () {
	echo -n "6448"
}
EOF

${ds} -p > funcs
ret=$?

echo > funcs.ok
echo bar >> funcs.ok
echo baz >> funcs.ok
echo foo >> funcs.ok
echo gronk >> funcs.ok

diff funcs funcs.ok || ret=1

rm -rf "${dir}"
exit ${ret}
