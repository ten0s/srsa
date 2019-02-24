#!/bin/bash -e

for file in $(find . -name '*.base64' | sort); do
    base64 -d $file > $(basename $file .base64)
done

for file in $(find . -name '*.tar.gz' | sort); do
    tar xfz $file
    rm -f $file
done

file=$1
module=$(basename $file .erl)
erlc -pa ./lib/*/ebin $file && erl -noshell -pa ./lib/*/ebin -eval "$module:main([])."
