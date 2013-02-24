#!/bin/bash

#--------------------------------------------------------------------
#   Setup
#--------------------------------------------------------------------
DOTFILES=$HOME/.dotfiles

#--------------------------------------------------------------------
#   Program Installs / Requirements Setup
#--------------------------------------------------------------------
# vim
# zsh
# htop
# xmonad
# tmux
# urxvt
# dzen2
# conky
# dmenu

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
ln -s $DOTFILES/vim/.vimrc $HOME/.vimrc
ln -s $DOTFILES/xorg/xinitrc $HOME/.xinitrc
ln -s $DOTFILES/xorg/Xresources $HOME/.Xresources
ln -s $DOTFILES/zshrc $HOME/.zshrc
