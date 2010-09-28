#! /bin/bash
erlc *.erl

echo "in smp enable..."
erl -noinput -smp enable -eval "io:format(\"~p\", [spawntest:serial_spawn(1)]), erlang:halt(0)"
echo "/s"


echo "in smp disable..."
erl -noinput -smp disable -eval "io:format(\"~p\", [spawntest:serial_spawn(1)]), erlang:halt(0)"
echo "/s"
