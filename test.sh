#!/bin/bash
ulimit -s 1000000
test_addR_header() {
    cat <<EOF
mixfixr 10 _+_ ;;
let _+_ = fun (a b : int) => \$add a b ;;
let a = 1 ;;
EOF
}
test_addR_body() {
    n=$1
    echo -n "a"
    for i in $(seq 1 $n); do
        echo -n " + a"
    done
    echo ";;"
}
test_addL_header() {
    cat <<EOF
mixfixl 10 _+_ ;;
let _+_ = fun (a b : int) => \$add a b ;;
let a = 1 ;;
EOF
}
test_addL_body() {
    n=$1
    echo -n "a"
    for i in $(seq 1 $n); do
        echo -n " + a"
    done
    echo ";;"
}
test_closed_header() {
    cat <<EOF
mixfix 10 A_B ;;
let A_B = fun (a : int) => \$add a 1 ;;
EOF
}
test_closed_body() {
    n=$1
    echo -n "A"
    for i in $(seq 1 $n); do
        echo -n " A"
    done
    echo -n " 1 B"
    for i in $(seq 1 $n); do echo -n " B"; done
    echo ";;"
}
test_prefix_header() {
    cat <<EOF
mixfix 10 ~_ ;;
let ~_ = fun (a : int) => \$add a 1 ;;
EOF
}
test_prefix_body() {
    n=$1
    for i in $(seq 1 $n); do echo -n "~ "; done
    echo "1 ;;"
}
test_postfix_header() {
    cat <<EOF
mixfix 10 _! ;;
let _! = fun (a : int) => \$add a 1 ;;
EOF
}
test_postfix_body() {
    n=$1
    echo -n "1"
    for i in $(seq 1 $n); do
        echo -n " !"
    done
    echo ";;"
}
test_addL2_header() {
    cat <<EOF
mixfixl 10 _A_B_ ;;
let _A_B_ = fun (a b c : int) => \$add (\$add a b) c ;;
let a = 1 ;;
EOF
}
test_addL2_body() {
    n=$1
    echo -n "a"
    for i in $(seq 1 $n); do
        echo -n " A a B a"
    done
    echo ";;"
}
test_addL3_header() {
    cat <<EOF
mixfixl 10 _A_B_C_ ;;
let _A_B_C_ = fun (a b c d : int) => \$add (\$add a b) (\$add c d);;
let a = 1 ;;
EOF
}
test_addL3_body() {
    n=$1
    echo -n "a"
    for i in $(seq 1 $n); do
        echo -n " A a B a C a"
    done
    echo ";;"
}

generate_test_file() {
    test_name=$1
    n=$2
    filename=$3
    eval "test_${test_name}_header" > $filename
    eval "test_${test_name}_body $n" >> $filename
}

bisect_n () {
    testname=$1
    a=$2
    b=$3
    while [ $a -lt $b ]; do
        n=$((a + (b - a) / 2))
        filename="test_$testname.mixfix"
        generate_test_file $testname $n $filename
        ./mixfix.exe -l $filename > /dev/null
        # If it fails, then the bug is in the left half
        if [ $? -ne 0 ]; then
            b=$n
            echo "Failed for n=$n" >&2
        else
            a=$((n + 1))
            echo "Passed for n=$n" >&2
        fi
    done
    # // Round down to 1000
    echo $((a / 1000 * 1000))
}

PARAMS_START=125000
PARAMS_STEP=$PARAMS_START
PARAMS_END=500000
TESTS="addR addL closed prefix postfix addL2 addL3"

# By 1000 untill 3000000
PARAM_NS=$(seq $PARAMS_START $PARAMS_STEP $PARAMS_END)
BASE_TESTS_DIR="/tmp/mixfix_test"
gen_all_test_files () {
    mkdir -p $BASE_TESTS_DIR
    testname=$1
    for n in ${PARAM_NS[@]}; do
        filename="$BASE_TESTS_DIR/${testname}_$n.mixfix"
        # Check if not exists
        if [ ! -f $filename ]; then
            echo "Generating $filename" >&2
            generate_test_file $testname $n $filename
        fi
        echo $filename
    done
}

export OCAMLRUNPARAM=l=8000M
binaries="1"
for testname in $TESTS; do
for bin in $binaries; do
    # Join all the params with commas
    gen_all_test_files $testname > /dev/null
    export_file="test/perf${bin}_${testname}.json"
    if [ -f $export_file ]; then
        echo "Skipping $testname" >&2
        continue
    fi
    params="-P num $PARAMS_START $PARAMS_END -D $PARAMS_STEP"
    hyperfine --warmup 1 -m 5 -M 6 -N $params -u microsecond --export-json ${export_file} -- "./mixfix${bin}.exe -l $BASE_TESTS_DIR/${testname}_{num}.mixfix -n"
done
cat test/perf*_${testname}.json | jq '{results:[inputs.results.[]|.]}' -n > test/all_${testname}.json 
done
