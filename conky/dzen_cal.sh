(echo "^fg(#ffffff)$date"; echo "$(cal -1)"; sleep 5) | dzen2 -xs 1 -w 110 -l 10 -ta r -e 'onstart=uncollapse,hide;button1=exit;button3=exit'
