{ inputs, pkgs, ... }:
{
  wayland.windowManager.hyprland = {
    plugins = with inputs; [
      hyprland-plugins.packages.${pkgs.system}.hyprexpo
      hyprland-plugins.packages.${pkgs.system}.hyprscrolling
    ];
    settings = {
      plugin = {
        hyprscrolling = {
          column_width = 0.45;
          fullscreen_on_one_column = true;
          focus_fit_method = 1;
        };
        hyprexpo = {
          columns = 3;
          gap_size = 5;
          bg_col = "rgb(111111)";
          workspace_method = "center current";
          gesture_distance = 300;
        };
      };
    };
  };
}
