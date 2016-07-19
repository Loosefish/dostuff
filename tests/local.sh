#!/bin/bash
# call local funtion in parent dir
ds=$(realpath "$1")
dir=$(mktemp -d)

cd "${dir}"

cat > Dofile << EOF
#!/bin/sh
do_foo () { # do_local
	pwd
}
EOF

mkdir child
cd child

out=$(${ds} foo)
ret=$?
if [ "$out" != "${dir}/child" ]; then
	ret=1
fi

rm -rf "${dir}"
exit ${ret}
