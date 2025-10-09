{ inputs, pkgs, ... }:
{
  wayland.windowManager.hyprland = {
    plugins = with inputs; [
      hyprland-plugins.packages.${pkgs.system}.hyprexpo
      hyprland-plugins.packages.${pkgs.system}.hyprscrolling
    ];
    # settings = {
    #   plugin = { };
    # };
  };
}
