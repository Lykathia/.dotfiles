#!/bin/bash
# Inspired by nnoell

CRIT="#99cc66"
BAR_FG="#3955c4"
BAR_BG="#363636"
DZEN_FG="#9d9d9d"
DZEN_FG2="#444444"
DZEN_BG="#020202"
COLOR_SEP=$DZEN_FG2
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"

seperator() {
    echo -n " ^fg($COLOR_SEP)|^fg()"
    return
}

printVolInfo() {
    Perc=$(amixer get Master | grep "Mono:" | awk '{print $4}' | tr -d '[]%')
    Mute=$(amixer get Master | grep "Mono:" | awk '{print $6}')
    echo -n "^fg($DZEN_FG2) ^ca(1,$VOL_TOGGLE_CMD)^ca(4,$VOL_UP_CMD)^ca(5,$VOL_DOWN_CMD)VOL^ca()^ca()^ca() "
    if [[ $Mute == "[off]" ]]; then
        echo -n "$(echo $Perc | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
        echo -n "^fg()off"
    else
        echo -n "$(echo $Perc | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
        echo -n "^fg()${Perc}%"
    fi
    return
}

mpdInfo() {
    while true; do
        printVolInfo
        seperator
    done
    return
}

conky -c $CONKYRC -u $UPDATE | mpdInfo | dzen2 -x 0 -y 0 -ta 'l' -p -e '' 
