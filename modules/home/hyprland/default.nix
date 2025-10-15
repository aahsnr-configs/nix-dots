{ inputs, pkgs, ... }:
{
  wayland.windowManager.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    xwayland.enable = true;
    systemd.enable = true;
  };

  imports = [
    ./monitor.nix
    ./colors.nix
    ./keybindings.nix
    ./settings.nix
    ./misc.nix
    ./env.nix
    ./rules.nix
    ./plugins.nix
    ./execs.nix
  ];
}
