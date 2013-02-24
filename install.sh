# Install some nice/required programs if we don't have them on the system...
# Required
# zsh
# vim

# Optional
# if mpd : ncmpcpp
# htop


#if not $HOME/xmonad
mkdir $HOME/.xmonad
ln -s $HOME/.xmonad/xmonad.hs $HOME/.dotfiles/xmonad.hs

ln -s $HOME/.vimrc $HOME/.dotfiles/vim/.vimrc
ln -s $HOME/.xinit $HOME/.dotfiles/xorg/xinitrc
ln -s $HOME/.Xresources $HOME/.dotfiles/xorg/Xresources
ln -s $HOME/.zshrc $HOME/.dotfiles/zshrc
