{ pkgs, lib, config, inputs, ... }: {
  services.hypridle = {
    enable = true;
    package = inputs.hypridle.packages.${pkgs.system}.hypridle;

    settings = {
      # Settings for global behavior.
      general = {
        lock_cmd = "${lib.getExe config.programs.hyprlock.package}";
        unlock_cmd = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
        before_sleep_cmd = "${pkgs.systemd}/bin/loginctl lock-session";
        after_sleep_cmd = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";

        # Set to true to ignore idle-inhibit requests from applications (e.g., Firefox, Steam).
        # ignore_dbus_inhibit = false;

        # Defaults to false.
        # ignore_systemd_inhibit = false;
      };

      listener = [
        {
          timeout = 150;
          on-timeout = "${pkgs.brightnessctl}/bin/brightnessctl -s set 10";
          on-resume = "${pkgs.brightnessctl}/bin/brightnessctl -r";
        }
        {
          timeout = 3000;
          on-timeout = "${pkgs.systemd}/bin/loginctl lock-session";
        }
        {
          timeout = 3300;
          on-timeout = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
          on-resume = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
        }
        {
          timeout = 10000;
          on-timeout = "${pkgs.systemd}/bin/systemctl hibernate";
        }
      ];
    };
  };
}
