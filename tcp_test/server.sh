#! /bin/bash
if !(erlc *.erl); then
    echo "compile source file error!"
    exit 1
fi

OPTS="-smp auto +K true +h 99999 "

ulimit -n 100000
erl $OPTS -noshell -name $SRV -eval "tcp_server:start()"
