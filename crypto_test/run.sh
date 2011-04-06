#! /bin/bash
erlc *.erl

erl -noinput -smp enable -eval "crypto_test:run(), erlang:halt(0)"
