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

if ${ds} foobar 2>/dev/null; then
	ret=1
else
	ret=0
fi

rm -rf "${dir}"
exit ${ret}
