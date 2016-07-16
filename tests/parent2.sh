#!/bin/bash
# call funtion in parent dir with Dofile in child dir
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

cat > Dofile << EOF
#!/bin/sh
do_foo () {
	echo -n "6446"
}
EOF


mkdir child
cd child

cat > Dofile << EOF
#!/bin/sh
do_bar () {
	echo -n "6447"
}
EOF


out=$(${ds} foo)
ret=$?
if [ "$out" != "6446" ]; then
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
