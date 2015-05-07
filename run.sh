#!/bin/bash

echo "Start at $(date)" >> logs/run.log

java -jar selenium-server-standalone-2.42.2.jar -Dwebdriver.chrome.bin=/usr/bin/google-chrome -DwebDriver.chrome.driver=./chromedriver &

sleep 5

./dist/dist-sandbox-fb615121/build/Fetcher/Fetcher  >>logs/fetcher.log 2>&1

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

./dist/dist-sandbox-fb615121/build/Markdown/Markdown >>logs/markdown.log 2>&1

if [ $? -ne 0 ]; then
    echo "MarkdownWriter failed" >> logs/run.log
    exit 1
fi

echo "Exit code of MarkdownWriter: $?" >> logs/run.log

./move-markdown.sh

echo "Stop at $(date)" >> logs/run.log

