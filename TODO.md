# Setting Up NixOS

## Insipirations

---
- [ ] [**blackdonos**](https://gitlab.com/theblackdon/black-don-os)
- [ ] [**zaneyos**](https://gitlab.com/Zaney/zaneyos)

- [x] app2unit is not installed
- [x] fish is not installedWSASD
- [x] Hyprland keybindings not working as expected
- [x] Caelestia is not loading wallpapers and gifs

## Main Goal
---

**_Main goal is to create a NixOS distribution with the following properties:_**

- **Make it as stable as possible**
- **Make it as optimized as possible**
- **Make it security hardened**

## TODOs

---

- [ ] Setup Apparmor in NixOS according to
      https://github.com/AaronVerDow/nix/tree/main/common/apparmor
- [ ] systemd user services
- [ ] Change fonts to source code pro fonts for editors and terminal, and keep
      rubik for ui
- [ ] fix network

## If you want to use determine nix

```sh
sudo nixos-rebuild boot --option extra-substituters https://install.determinate.systems --option extra-trusted-public-keys cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM= --flake .#zephyrus
```

# TODO niri

- [ ] setup latop dynamic monitors using laptop lid closed and open
- [ ] add environment variables
- [ ] add systemd user service for nix-switcher-daemon
- [ ] add environmental variables directly to system module
