#!/bin/bash
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

cat > Dofile << EOF
#!/bin/sh
do_foo () {
	echo -n "6446"
}
EOF

out=$(${ds} foo)
ret=$?
if [ "$out" != "6446" ]; then
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
