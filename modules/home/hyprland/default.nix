{ config, inputs, pkgs, ... }:

{
  wayland.windowManager.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    xwayland.enable = true;
    # systemd = {
    #   enable = true;
    #   variables = ["--all"];
    # };
    systemd.enable = false;
  };

  imports = [
    ./monitor.nix
    ./settings.nix
    ./keybindings.nix
    ./misc.nix
    ./plugins.nix
    ./rules.nix
    ./autostart.nix
  ];
}
