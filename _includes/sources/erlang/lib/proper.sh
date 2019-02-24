#!/bin/bash -e

VERSION=1.3
git clone https://github.com/proper-testing/proper --branch v$VERSION
(cd proper; make)
tar cz proper/{COPYING,ebin/*,include/*} | base64 > proper-$VERSION.tar.gz.base64
rm -rf proper
