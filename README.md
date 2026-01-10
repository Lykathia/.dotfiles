.dotfiles
=======

This repository contains an Ansible playbook and roles to set up a consistent development
environment.

Features:
- ZSH as default shell, with FZF integration
- Neovim
- Modern shell productivity tools (rg, fzf, sd, bat, etc)
- SSH keypair generation if not present
- BSPWM desktop environment
- tmux

Install
-------

On Arch, `git`, `yay` and `keybase` are handled separetly.

```
yay -Syu ansible keybase
keybase login
ansible-playbook playbook.yml -K
yay -S sddm-silent-theme ashell
```

If `~/.ssh/id_ed25519` does not exist, one will be generated for importing into GitHub.

Inspiration
-----------
* https://github.com/windelicato/dotfiles/
* https://github.com/jdost/config/
