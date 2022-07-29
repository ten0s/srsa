#!/bin/bash

# https://github.com/Rob--W/cors-anywhere/issues/301

curl -XPOST \
     -H "X-Requested-With: localhost" \
     -H "Content-Type: application/json" \
     -H "Authorization: Token ${GLOTIO_TOKEN?}" \
     https://cors-anywhere.herokuapp.com/https://glot.io/api/run/python/latest -d '{"files":[{"name":"list_length.py","content":"print(\"Hello\")\n"}],"command":"python list_length.py"}'
