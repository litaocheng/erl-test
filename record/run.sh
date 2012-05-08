#! /bin/bash
erlc +\'E\' *.erl || exit 1
erlc +\'S\' *.erl || exit 1
erlc +\'P\' *.erl || exit 1
erlc *.erl || exit 1
#rm -rf *.S *.dump *.P *.beam
