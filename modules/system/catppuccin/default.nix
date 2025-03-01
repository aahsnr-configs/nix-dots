{ config, pkgs, catppuccin, ... }:
{
  catppuccin = {
    cache.enable = true;
    tty = {
      enable = true;
      flavor = "macchiato";
    };
    # sddm = {
    #   enable = true;
    #   #background = "$HOME/nix-dots/modules/system/sddm/background.png";
    #   flavor = "macchiato";
    #   font = "JetBrainsMono Nerd Font";
    #   fontSize = "13";
    # };
  };
}
