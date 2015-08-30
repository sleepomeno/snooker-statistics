#!/bin/bash

cd /home/greg/haskell/snooker-statistics/ 
echo "Start at $(date)" >> logs/run.log

java -jar selenium-server-standalone-2.47.1.jar -Dwebdriver.chrome.bin=/usr/bin/google-chrome -DwebDriver.chrome.driver=./chromedriver &

sleep 5

SnookFetcher  >>logs/fetcher.log 2>&1

if [ $? -ne 0 ]; then
    echo "Fetcher failed" >> logs/run.log
    kill $(ps aux | grep '[w]ebdriver' | awk '{print $2}') &
    kill $(ps aux | grep '[c]hromedriver' | awk '{print $2}') &
    kill $(ps aux | grep '[c]hrome --disable-background-networking' | awk '{print $2}') &

    exit 1
fi

kill $(ps aux | grep '[w]ebdriver' | awk '{print $2}') &
kill $(ps aux | grep '[c]hromedriver' | awk '{print $2}') &
kill $(ps aux | grep '[c]hrome --disable-background-networking' | awk '{print $2}') &

SnookMarkdown >>logs/markdown.log 2>&1

if [ $? -ne 0 ]; then
    echo "MarkdownWriter failed" >> logs/run.log
    exit 1
fi

echo "Exit code of MarkdownWriter: $?" >> logs/run.log

./move-markdown.sh

echo "Stop at $(date)" >> logs/run.log

