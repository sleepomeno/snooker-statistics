#!/bin/bash

cd hakyll-frontent
rm -rf _site
rm -rf _cache
./.cabal-sandbox/bin/site build 

rm -rf /home/greg/octopress-master/desire/*

cp -R _site/* /home/greg/octopress-master/desire/

cd /home/greg/octopress-master
git add desire
git commit -m "Update desire at $(date)"
git push

cd ../
