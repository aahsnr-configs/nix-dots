{ inputs, pkgs, ... }:
{
  wayland.windowManager.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    xwayland.enable = true;
    systemd.enable = false;
  };

  imports = [
    ./monitor.nix
    ./settings.nix
    ./misc.nix
    ./plugins.nix
    ./rules.nix
    ./execs.nix
    ./keybindings.nix
    ./colors.nix
  ];
}
