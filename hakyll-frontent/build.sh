#!/bin/bash

rm -rf _site
rm -rf _cache
./.cabal-sandbox/bin/site clean
./.cabal-sandbox/bin/site build 
./.cabal-sandbox/bin/site server
