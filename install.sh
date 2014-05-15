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
# xcompmgr
# hsetroot

git submodule init
git submodule update

#--------------------------------------------------------------------
#   Xmonad
#--------------------------------------------------------------------
mkdir -p $HOME/.xmonad/icons

# Move icons for xmonad
for file in $DOTFILES/icons/*/*.xbm; do
    ln -s $file $HOME/.xmonad/icons/$(basename "$file")
done
ln -s $DOTFILES/xmonad.hs $HOME/.xmonad/xmonad.hs

#--------------------------------------------------------------------
#   Dotfiles
#--------------------------------------------------------------------
mkdir -p $HOME/.ncmpcpp/
ln -s $DOTFILES/vim $HOME/.vim
ln -s $DOTFILES/mutt $HOME/.mutt
ln -s $DOTFILES/fonts $HOME/.fonts
ln -s $DOTFILES/colours $HOME/.colours

ln -s $DOTFILES/tmux.conf $HOME/.tmux.conf
ln -s $DOTFILES/vim/vimrc $HOME/.vimrc
ln -s $DOTFILES/xorg/xinitrc $HOME/.xinitrc
ln -s $DOTFILES/xorg/Xresources $HOME/.Xresources
ln -s $DOTFILES/zshrc $HOME/.zshrc
ln -s $DOTFILES/zshenv $HOME/.zshenv
ln -s $DOTFILES/mutt/muttrc $HOME/.muttrc
ln -s $DOTFILES/mutt/offlineimaprc $HOME/.offlineimaprc
