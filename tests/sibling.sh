#!/bin/bash
# call function with same name in a child dir
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

mkdir right
cd right

cat > Dofile << EOF
#!/bin/sh
do_foo () {
	echo -n "right"
}
EOF

cd ..

mkdir left
cd left

cat > Dofile << EOF
#!/bin/sh
do_foo () {
	echo -n "left"
}
EOF

out=$(${ds} foo)
ret=$?
if [ "$out" != "left" ]; then
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
