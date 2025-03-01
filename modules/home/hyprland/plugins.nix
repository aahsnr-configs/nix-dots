{ inputs, pkgs, ... }:

{
  wayland.windowManager.hyprland = {
    plugins = with inputs; [
      hyprland-plugins.packages.${pkgs.system}.hyprexpo
      hy3.packages.${pkgs.system}.hy3
      Hyprspace.packages.${pkgs.system}.Hyprspace
    ];
    settings = {
      plugin = {
        hy3 = {
          tabs = {
            height = 2;
            padding = 6;
            render_text = false;
          };
          autotile = {
            enable = true;
            trigger_width = 800;
            trigger_height = 500;
          };
        };
        # overview = {
        #   autoDrag = true;
        #   autoScroll = true;
        #   exitOnClick = true;
        #   switchOnDrop = true;
        # };
      };
    };
  };
}
