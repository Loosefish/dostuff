#!/bin/bash
# call function with arg
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

cat > Dofile <<EOF
#!/bin/sh
do_foo () {
	env | grep DOSTUFF_CWD
}
EOF

out=$(${ds} foo)
ret=$?
if [ "$out" != "DOSTUFF_CWD=$PWD" ]; then
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
