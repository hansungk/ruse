#!/bin/sh

GREEN="\033[0;32m"
RED="\033[0;31m"
RS="\033[0m"

outputfile=/tmp/ruse.out
outputlinefile=/tmp/ruse.out.lines
truthlinefile=/tmp/ruse.truth.lines

_cleanup() {
    rm -f $outputfile
    rm -f $outputlinefile
    rm -f $truthlinefile
}

_pass() {
    _cleanup
    echo "${GREEN}PASS${RS} $1"
    exit 0
}

_fail() {
    _cleanup
    echo "${RED}FAIL${RS} $1"
    exit 1
}

_errordiff() {
    grep "error:" /tmp/ruse.out | cut -d':' -f2 >$outputlinefile
    grep -n "// error" $1 | cut -d':' -f1 >$truthlinefile
    diff /tmp/ruse.out.lines /tmp/ruse.truth.lines || _fail $1
    _pass $1
}

test() {
    local ret=0
    local header=$(head -n1 $1)
    echo ${header} | grep -q "fail" && ret=1
    if echo ${header} | grep -q "exit"
    then
        ret=$(echo ${header} | awk '{ print $3 }')
    fi

    ./ruse $1 >$outputfile 2>&1

    if [ $? -ne $ret ]
    then
        _fail $1
    else
        _errordiff $1
    fi
}

test test/decl.ruse
