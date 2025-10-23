{ inputs, pkgs, ... }:
{
  wayland.windowManager.hyprland = {
    plugins = with inputs; [
      hyprland-plugins.packages.${pkgs.system}.hyprexpo
      hyprland-plugins.packages.${pkgs.system}.hyprscrolling
    ];
    # plugin = {
    #   hyprscrolling = {
    #     column_width = 0.7;
    #     fullscreen_on_one_column = false;
    #   };
    # };
  };
}
