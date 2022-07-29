#!/bin/bash

# Run $ bin/cors-anywhere at first

curl -XPOST \
     -H "X-Requested-With: localhost" \
     -H "Content-Type: application/json" \
     -H "Authorization: Token ${GLOTIO_TOKEN?}" \
     http://localhost:8080/https://glot.io/api/run/python/latest -d '{"files":[{"name":"list_length.py","content":"print(\"Hello\")\n"}],"command":"python list_length.py"}'
