{ inputs, pkgs, ... }:
{
  wayland.windowManager.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    xwayland.enable = true;
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };
  };

  imports = [
    ./monitor.nix
    ./settings.nix
    ./keybindings.nix
    ./misc.nix
    ./plugins.nix
    ./rules.nix
    ./wsaction.nix
  ];
}
