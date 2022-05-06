#!/bin/sh

GREEN="\033[0;32m"
RED="\033[0;31m"
RS="\033[0m"

outputfile=/tmp/ruse.out
outputlinefile=/tmp/ruse.out.lines
truthlinefile=/tmp/ruse.truth.lines
difffile=/tmp/ruse.diff

_cleanup() {
    rm -f $outputfile
    rm -f $outputlinefile
    rm -f $truthlinefile
    rm -f $difffile
}

_pass() {
    echo "${GREEN}PASS${RS} $1"
}

_fail() {
    echo "${RED}FAIL${RS} $1"
}

_checkerror() {
    grep "error:" /tmp/ruse.out | cut -d':' -f2 >$outputlinefile
    grep -n "// error" $1 | cut -d':' -f1 >$truthlinefile
    if diff $outputlinefile $truthlinefile >$difffile
    then
        _pass "$1"
    else
        _fail "$1"
        cat $difffile
        echo "< got, > want"
    fi
}

_test() {
    ret=0
    header=$(head -n1 $1)
    echo ${header} | grep -q "fail" && ret=-1
    if echo ${header} | grep -q "run"
    then
        ret=$(echo ${header} | awk '{ print $3 }')
    fi

    if ./ruse $1 >$outputfile 2>&1
    then
        ./out
        retgot="$?"
    else
        retgot=-1
    fi

    if [ $retgot -eq $ret ]
    then
        _checkerror "$1"
    else
        _fail "$1"
    fi
    _cleanup
}

if ! [ -x "ruse" ]; then
    echo "ruse not built"
    exit 1
fi

[ -n "$1" ] && _test "$1" && exit

_test test/simple.ruse
_test test/decl.ruse
_test test/struct.ruse
_test test/ref.ruse
_test test/func.ruse
_test test/gen_ref.ruse
_test test/gen_func.ruse
_test test/gen_struct.ruse
