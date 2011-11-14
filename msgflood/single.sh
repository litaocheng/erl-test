#! /bin/bash
if !(erlc *.erl); then
    echo "compile source file error!"
    exit 1
fi

OPTS="-smp auto +K true +h 99999 \
      -kernel inet_default_connect_options \
        [{nodelay,false},{recbuf,1024000},{sndbuf,10240000}] \
      -kernel inet_default_listen_options \
        [{nodelay,false},{recbuf,1024000},{sndbuf,10240000}]"

erl $OPTS -sname single -noshell -eval "spawn(fun() -> msgflood:server(flood) end), msgflood:client(flood)"
