# Setting Up NixOS
## Main Goal
***Main goal is to create a NixOS distribution with the following properties:***
  - __Make it as stable as possible__
  - __Make it as optimized as possible__
  - __Make it security hardened__

## TODOS
***
- [ ] Search through sidoomy-dotfiles to make NixOS stable
- [x] Add schizofox to home-manager
- [x] Move fonts to system
- [x] Generate a kitty hmModule
- [x] Add the recently updated anyrun as hmModule
- [x] Fill in the configs in the edited all-modules-modded.md
- [x] Use raexera yuki vscode home-manager
- [x] Add zen-browser as a flake
- [x] Add determinate nix flake
- [x] Adapt the git.nix module for NixOS
- [x] Check with Hyprland NixOS wiki if some env variables are necessary
- [ ] Make sure all modules, both home and system, use catppuccin nix
- [x] Make sure to enable uwsm in hyprland
- [ ] add a background for regreet, caelestia shell and hyprlock
- [ ] Setup fuzzel as home-manager: possibly borrow from Caelestia
- [x] Write systemd user units for `caelestia resizer -d` and `caelestia shell -d`
- [ ] Re-edit `system/openssh/default.nix` after everything
- [ ] Apply the gemini gcc15 fix temporarily and print the error output to a different gemini account: Use this method to use from gcc14 to gcc15.
- [ ] Also try to find out if emacs-overlay uses libgccjit15: if no, introduce your own
- [ ] Install cachyos-lto kernel from nyx when everything is stable and gcc15 is the default compiler

## Hyprland Plugins and Tools

***
[Note]: _Don't need to use both hyprscroller and hyprscrolling_
[Note]: _Don't need to use both hyprexpo and Hyprspace_

- [ ] [**hyprscoller**](https://github.com/cpiber/hyprscroller)
- [ ] [**hyprexpo & hyprscrolling**](https://github.com/hyprwm/hyprland-plugins)
- [ ] [**pyprland**](https://github.com/hyprland-community/pyprland)

## Doom Emacs TODOS
- [ ] For python programming, check $HOME/git-repos/work/python-programming/notes/recommended-tools.md
- [ ] For bash programming, check $HOME/git-repos/work/bash-programming/notes/tools.md

## Home Modules

***

- [x] anyrun
- [x] atuin
- [x] bat
- [x] btop
- [ ] caelestia
- [ ] catppuccin
- [x] cliphist
- [x] dev
- [x] emacs
- [x] emoji
- [x] eza
- [x] fd-find
- [x] fzf
- [x] git
- [x] hypridle
- [x] hyprland
- [x] hyprlock
- [x] hyprpaper
- [x] hyprpolkitagent
- [x] hyprsunset
- [x] imv
- [x] keyring
- [x] kitty
- [ ] lazygit
- [x] mpv
- [x] pay-respects
- [x] pkgs
- [x] pyprland
- [x] ripgrep
- [x] starship
- [ ] systemd user services
- [x] texlive
- [x] tldr
- [x] theming
- [x] tmux
- [x] xdg
- [x] yazi
- [x] zathura
- [x] zoxide
- [x] zsh

## System Modules

***

- [x] asus
- [x] bluetooth
- [x] catppuccin
- [x] clamav
- [x] dbus
- [x] environment
- [x] fonts
- [x] graphics
- [x] greetd
- [x] hyprland
- [ ] intel
- [x] kernel
- [x] logind
- [x] misc
- [x] network
- [x] nix
- [x] openssh
- [x] pkgs
- [x] portal
- [x] seatd
- [x] security
- [x] shell
- [x] sound
- [x] systemd
- [x] thunar
- [x] users
- [x] virtualization
- [x] zram


## New Flake Inputs

***

- [x] pyprland
- [x] hyprland-plugins
- [x] quickshell
- [x] caelestia-dots/shell
- [x] FBIGlowie/zen-browser-flake

- sudo nixos-rebuild boot --option extra-substituters https://install.determinate.systems --option extra-trusted-public-keys cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM= --flake .#zephyrus
