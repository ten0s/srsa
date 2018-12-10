#!/bin/bash -e

ANKI_HOST=localhost
ANKI_PORT=8765
ANKI_CONNECT_VER=6
EXE_DIR=./exercises
EXE_URL="https://ten0s.github.io/srs/exercises"

if [[ $# -ne 1 ]]; then
    echo "Usage: $(basename $0) <all | NAME>"
    echo "Example:"
    echo "$ $(basename $0) AppendListErlang.md"
    exit 1
fi

nc -z $ANKI_HOST $ANKI_PORT || (echo "Anki is NOT running"; exit 1)

name=$1
exercise="$EXE_DIR/$name"
if [[ ! -f $exercise ]]; then
    echo "$exercise not found"
    exit 1
fi
echo "Adding $name to Anki..."
title=$(sed -En 's/title:\s*(.*)/\1/p' $exercise)
language=$(sed -En 's/language:\s*(.*)/\1/p' $exercise)

# https://foosoft.net/projects/anki-connect/#notes
curl $ANKI_HOST:$ANKI_PORT -X POST -d @- <<EOF
{
    "action": "addNote",
    "version": $ANKI_CONNECT_VER,
    "params": {
        "note": {
            "deckName": "Algorithms",
            "modelName": "Basic",
            "fields": {
                "Front": "<a href=\"${EXE_URL}/${name%.md}.html\" target=\"_top\">${title} in ${language^}</a>",
                "Back": ""
            },
            "options": {
                "allowDuplicate": false
            },
            "tags": [
                "$language"
            ]
        }
    }
}
EOF
echo
