#!/bin/bash

cd hakyll-frontent
rm -rf _site
rm -rf _cache
./.cabal-sandbox/bin/site build 

rm -rf /home/greg/octopress/_deploy/desire/*

cd /home/greg/octopress-master
git pull
cd -

cp -R _site/* /home/greg/octopress/_deploy/desire/

cd /home/greg/octopress/_deploy
git add desire
git commit -m "Update desire at $(date)"
git push

cd ../
