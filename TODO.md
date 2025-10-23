# Setting Up NixOS

[**Note**]: ahsan.05rahman@gmail.com for claude.ai is very important

- [ ] Before adding the simplified org setup, test org-mode with the default settings provided by the emacs packages.
- [x] Fix caelestia hmModule; make it look exactly like json file
- [x] Fix hyprland keybindings for caelestia
- [ ] Setup Apparmor in NixOS according to https://github.com/AaronVerDow/nix/tree/main/common/apparmor
- [x] app2unit is not installed
- [x] fish is not installed
- [x] Hyprland keybindings not working as expected
- [x] Caelestia is not loading wallpapers and gifs

## Main Goal

**_Main goal is to create a NixOS distribution with the following properties:_**

- **Make it as stable as possible**
- **Make it as optimized as possible**
- **Make it security hardened**

## TODOS

---

- [ ] thunar is not working apparently because it being sent to special workspaces
- [x] make sure the volume keybinding does not increase the volume more than 100 percent
- [x] turn org-capture bash script into fish script and integrate into scripts
- x ] add the new colorscheme from claude to doom emacs
- [ ] setup foot so that it always opens with zellij
- [ ] before switching to hyprscroller, setup and test hyprscrolling using the config from [black-don-os](https://gitlab.com/theblackdon/black-don-os)
- [ ] check back about needed packages for latex and org-mode
- [ ] add configs for latex and nix
- [ ] modify the org-modern config like my vanilla emacs config
- [ ] setup hyprscroller instead of hyprscrolling
- [ ] add [hyprshell](https://github.com/H3rmt/hyprshell)
- [ ] modify the hyprland configuration so that moving the cursor does not bring a window to focus; then make sure that when window focus is changed, the cursor follows the focused windows
- [ ] kickstart-nvim setup to port all my necessary Astronvim configurations slowly
- [x] scripts creation
- [x] gammastep home-manager configuration; remove hyprsunset
- [x] temporarily set the scheme color variables directly as hyprland home-manager modules in extraConfig option
- [x] Use claude opus to compare and contrast the config files in both the home-manager modules and the default hyprland config files
- [x] Port hyprland.conf settings to the execs.nix
- [x] Make sure scheme/default.conf is sourced by hyprland but also writable
- [x] **hypr-user.conf** and **hypr-vars.conf** are need to **sourced** by Hyprland itself.

- [x] Use config files for all hyprland files; make sure the `conf` folder is created by home-manager if not detected
- [x] Make sure foot with server hmModule is setup
- [x] Make sure udiskie is setup
- [x] Make sure hyprsunset hmModule is setup
- [x] Use caeletia conf file as well

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
- [x] Make sure all modules, both home and system, use catppuccin nix
- [x] Make sure to enable uwsm in hyprland
- [x] Setup fuzzel as home-manager: possibly borrow from Caelestia
- [x] Write systemd user units for `caelestia resizer -d` and `caelestia shell -d`
- [x] Re-edit `system/openssh/default.nix` after everything
- [x] Also try to find out if emacs-overlay uses libgccjit15: if no, introduce your own
- [ ] Install cachyos-lto kernel from nyx when everything is stable and gcc15 is the default compiler

## Hyprland Plugins and Tools

---

[Note]: _Don't need to use both hyprscroller and hyprscrolling_
[Note]: _Don't need to use both hyprexpo and Hyprspace_

- [ ] [**hyprscoller**](https://github.com/cpiber/hyprscroller)
- [ ] [**hyprexpo & hyprscrolling**](https://github.com/hyprwm/hyprland-plugins)
- [ ] [**pyprland**](https://github.com/hyprland-community/pyprland)

## Doom Emacs TODOS

- [ ] For python programming, check $HOME/git-repos/work/python-programming/notes/recommended-tools.md
- [ ] For bash programming, check $HOME/git-repos/work/bash-programming/notes/tools.md

## Home Modules

---

- [x] anyrun
- [x] atuin
- [x] bat
- [x] btop
- [x] caelestia
- [x] catppuccin
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
- [x] lazygit
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

---

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

---

- [x] pyprland
- [x] hyprland-plugins
- [x] quickshell
- [x] caelestia-dots/shell
- [x] FBIGlowie/zen-browser-flake

- sudo nixos-rebuild boot --option extra-substituters https://install.determinate.systems --option extra-trusted-public-keys cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM= --flake .#zephyrus
