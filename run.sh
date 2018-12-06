#!/bin/bash -e

if [[ $# -ne 1 ]]; then
    echo "Usage: $(basename $0) <all | NAME>"
    echo "Examples:"
    echo "$ $(basename $0) all"
    echo "$ $(basename $0) ArrayQueueJava.md"
    exit 1
fi

EXE_DIR=./exercises/
SRC_DIR=./_includes/sources/

function run() {
    local name=$1
    local exercise="$EXE_DIR/$name"
    local language
    local command
    if [[ ! -f $exercise ]]; then
        echo "$exercise not found"
        exit 1
    fi
    echo "Running $name..."
    language=$(sed -En 's/language:\s*(.*)/\1/p' $exercise)
    command=$(sed -En 's/command:\s*(.*)/\1/p' $exercise)
    pushd $SRC_DIR/$language/ >/dev/null
    bash -c "$command"
    popd >/dev/null
}

function run_all() {
    for path in $(find $EXE_DIR -name '*.md' | sort); do
        run $(basename $path)
    done
}

what=$1
case $what in
    "all") run_all;;
    *) run $what
esac
