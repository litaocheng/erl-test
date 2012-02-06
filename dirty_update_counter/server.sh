#! /bin/bash
if !(erlc *.erl); then
    echo "compile source file error!"
    exit 1
fi

SERVER=server@127.0.0.1

if [ $# -eq 1 ] && [ -n "$1" ]; then
    SERVER=$1
fi
echo "start the server"

COOKIE=test
OPTS="-smp auto +K true +h 99999"

erl $OPTS -setcookie $COOKIE -noshell -name $SERVER -eval "main:server()"
rm -rf *.dump *.beam
