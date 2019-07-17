#!/bin/sh

MATCH=`ls env/ | cut -d "." -f1  | uniq | paste -sd "|" -`;


if [ $# -lt 1 ]; then
    echo "Usage: ./start.sh {$MATCH}" >&2
    exit 1
fi
`echo "$MATCH" | grep -q $1`
if [ $? -ne 0 ]; then
    echo "Usage: ./start.sh {$MATCH}" >&2
    exit 1
fi

exec env ERL_LIBS=.:$PWD/deps erl -args_file env/${1}.vmargs -config env/${1} -pa ebin/ -pa deps/*/ebin -pa _build/${1}/deps/*/ebin
exit 0
