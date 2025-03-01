{ config, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      cursor = {
        unfocused_hollow = false;
        style = {
          blinking = "On";
          shape = "Block";
        };
      };
      env = {
        TERM = "alacritty";
        WINIT_X11_SCALE_FACTOR = "1.0";
      };
      font = {
        size = 22;
        builtin_box_drawing = true;
        offset = {
          x = 0;
          y = 0;
        };
      };
      window = {
        decorations = "full";
        dynamic_title = true;
        opacity = 0.85;
        startup_mode = "Windowed";
        dimensions = {
          columns = 82;
          lines = 23;
        };
        padding = {
          x = 10;
          y = 10;
        };
      };
    };
  };
}
