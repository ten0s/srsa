#!/bin/bash -e

for file in $(find . -name '*.base64' | sort); do
    base64 -d $file > $\{file%.base64\}
done

for file in $(find . -name '*.tar.gz' | sort); do
    tar xfz $file
done

file=$1
module=$\{file%.erl\}
erlc -pa ./*/ebin $file && erl -noshell -pa ./*/ebin -eval "$module:main([])."
