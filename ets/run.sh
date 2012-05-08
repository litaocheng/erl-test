#! /bin/bash
erlc *.erl || exit 1
erlc ../util/util.erl -o . || exit 1
usage() 
{
    echo "用法"
    echo "$0 条数"
    echo ""
}

if [ $# -eq 0 ]; then
    usage
    exit 1
fi
N=$1
erl -noshell -smp enable -eval "ets_test:run($N), init:stop(0)"
rm -rf *.beam *.dump
