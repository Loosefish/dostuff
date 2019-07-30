#!/bin/bash
# call function with arg
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

cat > Dofile <<EOF
#!/bin/sh
do_foo () {
	echo -n "\$1"
}
EOF

out=$(${ds} foo 6446)
ret=$?
if [ "$out" != "6446" ]; then
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
