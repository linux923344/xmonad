#!/bin/sh
localtion=Poznan
testweather() { 
[ "$(stat -c %y "/home/yorune/.weatherreport" 2>/dev/null | cut -d' ' -f1)" != "$(date '+%Y-%m-%d')" ] && ping -q -c 1 1.1.1.1 >/dev/null && curl -s "wttr.in/$location" > "/home/yorune/.weatherreport" 

#sed '16q;d' "/home/yorune/.weatherreport" | grep -wo "[0-9]*" | sort -n | sed -e '$!d' | tr -d '\n' | awk '{print "<fn=1></fn>",$1 "%"}' 
#sed '13q;d' "/home/yorune/.weatherreport" | grep -o "m\\(-\\)*[0-9]\\+" | sort -n -t 'm' -k 2n | sed -e 1b -e '$!d' | tr '\n|m' ' ' | awk '{print "<fn=1></fn>",$1 "°","<fn=1></fn>",$2 "°"}'

echo -e "$(sed '16q;d' "/home/yorune/.weatherreport" | grep -wo "[0-9]*" | sort -n | sed -e '$!d' | tr -d '\n') $(sed '13q;d' "/home/yorune/.weatherreport" | grep -o "m\\(-\\)*[0-9]\\+" | sort -n -t 'm' -k 2n | sed -e 1b -e '$!d' | tr '\n|m' ' ')" | awk '{print "<fn=1></fn>",$1 "%", "<fn=1></fn>",$2 "°","<fn=1></fn>",$3 "°"}'

#sed '13q;d' "/home/yorune/.weatherreport" | grep -o "m\\(-\\)*[0-9]\\+" | sort -n -t 'm' -k 2n | sed -e 1b -e '$!d' | tr '\n|m' ' ' | awk '{print " <fn=1>❄️</fn>",$1 "°","<fn=1>🌞</fn>",$2 "°"}'
}
testweather
