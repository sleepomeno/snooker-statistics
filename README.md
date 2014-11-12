snooker-statistics
==================

Parse snooker game results from gamedesire.

You need to have a running selenium process, so you need to download
the selenium server and a chrome driver for it from
http://www.seleniumhq.org/download/. Then start it like that:

java -jar selenium-server-standalone-2.42.2.jar  -Dwebdriver.chrome.bin=/usr/bin/google-chrome -DwebDriver.chrome.driver=./chromedriver

cabal install
cabal run
