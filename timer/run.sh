#! /bin/bash
erlc *.erl || exit 1
erl -noinput -smp enable -eval "timer_test:run(), erlang:halt(0)"
rm -rf *.beam *.dump
