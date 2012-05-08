#! /bin/bash
erlc *.erl || exit 1
erl -noshell -smp enable -eval "busy:run()"
rm -rf *.beam *.dump
