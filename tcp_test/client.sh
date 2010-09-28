#! /bin/bash
if !(erlc *.erl); then
    echo "compile source file error!"
    exit 1
fi

OPTS="-smp auto +K true +h 99999"

echo "start the tcp client"
erl $OPTS -noshell -eval "client_stat:run()"
