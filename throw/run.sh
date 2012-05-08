#! /bin/bash
erlc *.erl || exit 1
echo "throw:"
erl -noshell -smp enable -eval "throw:run(true), init:stop()"
echo "no throw:"
erl -noshell -smp enable -eval "throw:run(false), init:stop()"
rm -rf *.beam *.dump
