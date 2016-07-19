#!/bin/bash
# basic call of a named function
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

cat > xxx << EOF
#!/bin/sh
do_foo () {
	echo -n "6446"
}
EOF

cat > Dofile << EOF
#!/bin/sh
do_foo () {
	echo -n "6447"
}
EOF

out=$(DOFILE=xxx ${ds} foo)
ret=$?
if [ "$out" != "6446" ]; then
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
