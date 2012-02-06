#! /bin/bash
if !(erlc *.erl); then
    echo "compile source file error!"
    exit 1
fi

SERVER=server@127.0.0.1
CLIENT_NUM=1000

if [ $# -eq 1 ] && [ -n "$1" ]; then
    SERVER=$1
fi

COOKIE=test
OPTS="-smp auto +K true +h 99999 "

echo "server is $SERVER"
echo "start the $CLIENT_NUM client"
erl $OPTS -setcookie $COOKIE -noshell -name client_$SERVER -eval "main:client('${SERVER}', ${CLIENT_NUM})"
rm -rf *.dump *.beam
