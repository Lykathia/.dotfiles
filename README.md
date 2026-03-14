.dotfiles
=======

This repository contains an Ansible playbook and roles to set up a consistent development
environment. Supports both Arch Linux desktops and Ubuntu servers.

Features:
- ZSH as default shell, with FZF integration
- Neovim
- Modern shell productivity tools (rg, fzf, sd, bat, etc)
- SSH keypair generation if not present
- Desktop environment (Arch only atm)
- tmux

Install (Arch)
--------------

`git`, `yay` and `keybase` are handled separately.

```
yay -Syu ansible keybase
keybase login
ansible-playbook playbook.yml -K
yay -S sddm-silent-theme ashell
keybase pgp export --secret | gpg --allow-secret-key-import --import
```

Install (Ubuntu)
----------------

For workstations / sandboxes. Skip desktop.

```
sudo apt update && sudo apt install -y ansible git
ansible-playbook playbook.yml -K --skip-tags desktop
```

Notes
-----

If `~/.ssh/id_ed25519` does not exist, one will be generated for importing into GitHub.

Inspiration
-----------
* https://github.com/windelicato/dotfiles/
* https://github.com/jdost/config/
