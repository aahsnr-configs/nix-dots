{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  services.hypridle = {
    enable = true;
    package = inputs.hypridle.packages.${pkgs.system}.hypridle;

    settings = {
      general = {
        lock_cmd = lib.getExe config.programs.hyprlock.package;
        before_sleep_cmd = "loginctl lock-session";
        after_sleep_cmd = "hyprctl dispatch dpms on";
      };

      listener = [
        {
          timeout = 150;
          on-timeout = "brightnessctl -s set 10";
          on-resume = "brightnessctl -r";
        }
        {
          timeout = 3000;
          on-timeout = "loginctl lock-session";
        }
        {
          timeout = 3300;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
        {
          timeout = 10000;
          on-timeout = "systemctl hibernate";
        }
      ];
    };
  };
}
