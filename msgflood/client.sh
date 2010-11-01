#! /bin/bash
if !(erlc *.erl); then
    echo "compile source file error!"
    exit 1
fi

SRV=flood_srv@127.0.0.1

if [ $# -eq 1 ] && [ -n "$1" ]; then
    SRV=$1
fi

COOKIE=flood
OPTS="-smp auto +K true +h 99999 \
      -kernel inet_default_connect_options \
        [{nodelay,false},{recbuf,102400},{sndbuf,1024000}] \
      -kernel inet_default_listen_options \
        [{nodelay,false},{recbuf,102400},{sndbuf,1024000}]"

echo "flood server is $SRV"
echo "start the msg flood client"
erl $OPTS -setcookie $COOKIE -noshell -name client_$SRV -eval "msgflood:client('${SRV}', flood)"
