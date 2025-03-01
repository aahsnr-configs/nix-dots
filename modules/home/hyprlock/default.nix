{ inputs, pkgs, ...}:

{
  programs.hyprlock = {
    enable = true;
    package = inputs.hyprlock.packages.${pkgs.system}.hyprlock;
    settings = {
      general = {
        disable_loading_bar = true;
        hide_cursor = false;
      };

      background = [
        {
          monitor = "";
          path = "$HOME/nix-dots/modules/home/hyprlock/lockscreen.png";
          blur_passes = 4;
          blur_size = 4;
          new_optimizations = true;
          ignore_opacity = false;
        }
      ];

      input-field = [
        {
          monitor = "";
          size = "600, 100";
          outline_thickness = 1;
          outer_color = "rgb(24, 25, 38)";
          inner_color = "rgb(202, 211, 245)";
          font_color = "rgb(24, 25, 38)";
          check_color = "rgb(240, 198, 198})";
          fail_color = "rgb(237, 135, 150)";
          fade_on_empty = false;
          placeholder_text = "<i>Input Password...</i>";
          dots_spacing = 0.5;
          dots_center = true;
        }
      ];

      label = [
        {
          monitor = "";
          text = "cmd[update:3600000] date +'%a %b %d'";
          font_family = "JetBrainsMono Nerd Font";
          font_size = 17;
          color = "rgb(24, 25, 38)";
          position = "0, 50";
          valign = "center";
          halign = "center";
        }
      ];
    };
  };
}
