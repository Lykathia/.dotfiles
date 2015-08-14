typeset -U path
path=(~/bin ~/.cabal/bin $(ruby -rubygems -e "puts Gem.user_dir")/bin /usr/local/heroku/bin $path)

# bspwm + bar
export PANEL_FIFO="/tmp/panel_fifo"
export PANEL_HEIGHT=24
export PANEL_FONT_FAMILY="-*-terminus-medium-r-normal-*-12-*-*-*-c-*-*-1"
