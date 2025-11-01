{
  inputs,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    portalPackage = pkgs.xdg-desktop-portal-hyprland;
    xwayland.enable = true;
    systemd.enable = false;
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
