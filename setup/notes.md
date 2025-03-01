## Setup to gentoofy nixos
- [X] First fix whether flake.lock is not pushed to gitlab
- [X] Make sure substituters are disabled and hostPlatform is enabled
- [X] Change the nix package from nix-git to nix sth else
- [X] Remove nixos channel and add nixos-unstable to both sudo and shell, and, not false) since same result
- [X] Then execute 'sudo nix-channel --list' , 'add 10 second wait', 'sudo nix-channel --update', 'nix-channel --update'
- [ ] add extra-system-features to configuration.nix
- [ ] Make the necessary changes to configuration.nix in nix-dots using /etc/nixos/ contents
- [ ] copy over configuration.nix from nix-dots to /etc/nixos
- [ ] copy over hardware.nix from /etc/nixos to nix-dots
- [ ] Execute 'sudo nixos-rebuild boot --upgrader', and then reboot
- [ ] Execute 'u limit -n 50,000,000'
- [ ] Then start the install process with 'sudo nixos-rebuild boot --flake $HOME/nix-dots/#zephyrus --option substitute false --upgrade --verbose --max-jobs 2 --cores 13'

