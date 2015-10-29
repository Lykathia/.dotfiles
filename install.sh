#!/bin/bash

#--------------------------------------------------------------------
#   Setup
#--------------------------------------------------------------------
DOTFILES=`pwd`

#--------------------------------------------------------------------
#   Program Installs / Requirements Setup
#--------------------------------------------------------------------
# vim
# zsh
# htop
# xmonad
## dmenu
## xdotool
## dzen2
# tmux
# urxvt
# conky
# ctags

# AUR
# ~~~
# compton
# hsetroot

PACMAN="vim zsh htop dmenu xdotool dzen2 tmux ctags conky urxvt weechat trayer slock"
AUR="hsetroot typesafe-activator lemonbar-git compton xtitle-git"

# TODO: Loop thru these bitches and install them!
# TODO: Automatically install AUR modules as well

git submodule init
git submodule update


#--------------------------------------------------------------------
#   Dotfiles
#--------------------------------------------------------------------
mkdir -p $HOME/.ncmpcpp/
mkdir -p $HOME/.config/
ln -s $DOTFILES/vim $HOME/.vim
ln -s $DOTFILES/fonts $HOME/.local/share/fonts
ln -s $DOTFILES/colours $HOME/.colours

for folder in $DOTFILES/config/*; do
    ln -s ${folder} $HOME/.config/`basename ${folder}`
done

ln -s $DOTFILES/tmux.conf $HOME/.tmux.conf
ln -s $DOTFILES/vim/vimrc $HOME/.vimrc
ln -s $DOTFILES/xorg/xinitrc $HOME/.xinitrc
ln -s $DOTFILES/xorg/Xresources $HOME/.Xresources
ln -s $DOTFILES/zshrc $HOME/.zshrc
ln -s $DOTFILES/zshenv $HOME/.zshenv

fc-cache -vf
