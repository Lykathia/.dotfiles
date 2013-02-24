# Install some nice/required programs if we don't have them on the system...
# Required
# zsh
# vim

# Optional
# if mpd : ncmpcpp
# htop


# Xmonad
#if not $HOME/xmonad
mkdir $HOME/.xmonad
ln -s $HOME/.xmonad/xmonad.hs $HOME/.dotfiles/xmonad.hs

mkdir $HOME/.xmonad/icons
# Move icons for xmonad
for file in $HOME/.dotfiles/icons/*/*.xbm; do
    ln -s $file $HOME/.xmonad/icons/$(basename "$file").xbm
done

# Dotfiles
ln -s $HOME/.dotfiles/vim/.vimrc $HOME/.vimrc
ln -s $HOME/.dotfiles/xorg/xinitrc $HOME/.xinitrc
ln -s $HOME/.dotfiles/xorg/Xresources $HOME/.Xresources
ln -s $HOME/.dotfiles/zshrc $HOME/.zshrc
