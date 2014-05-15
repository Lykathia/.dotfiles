typeset -U path
path=(~/bin ~/.cabal/bin $(ruby -rubygems -e "puts Gem.user_dir")/bin /usr/local/heroku/bin $path)
