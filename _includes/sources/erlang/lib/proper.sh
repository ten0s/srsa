#!/bin/bash -e

VERSION=1.3
git clone https://github.com/proper-testing/proper lib/proper --branch v$VERSION
(cd lib/proper; make)
tar cz lib/proper/{COPYING,ebin/*,include/*} | base64 > proper-$VERSION.tar.gz.base64
rm -rf lib
