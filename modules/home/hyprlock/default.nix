# ~/.config/nixpkgs/home-manager/hyprlock.nix
{ pkgs, inputs, ... }: {
  programs.hyprlock = {
    enable = true;
    package = inputs.hyprlock.packages.${pkgs.system}.hyprlock;

    settings = {
      general = {
        disable_loading_bar = true;
        hide_cursor = true;
        grace = 2;
        pam_module = "hyprlock";
      };

      background = [{
        path = "./background.png";
        blur_passes = 3;
        blur_size = 12;
        noise = 1.17e-2;
        contrast = 1.2;
        brightness = 0.85;
        vibrancy = 0.2;
        vibrancy_darkness = 0.0;
      }];

      image = {
        path = "./face";
        size = 180;
        rounding = -1;
        border_size = 4;
        position = "0, 200";
        halign = "center";
        valign = "center";
      };

      input-field = {
        size = "480, 75";
        outline_thickness = 3;
        dots_size = 0.35;
        dots_spacing = 0.35;
        dots_center = true;
        dots_rounding = -1;
        fade_on_empty = true;
        fade_timeout = 1000;
        placeholder_text = "<i>Enter Password</i>";
        hide_input = false;
        rounding = 18;
        fail_text = "<i>Authentication Failed</i>";
        fail_timeout = 2000;
        fail_transitions = 300;
        position = "0, -30";
        halign = "center";
        valign = "center";
      };

      # Each label is an element in this list.
      label = [
        {
          text = "$USER";
          font_size = 36;
          font_family = "JetBrainsMono Nerd Font";
          position = "0, 100";
          halign = "center";
          valign = "center";
        }
        {
          text = ''
            cmd[update:1000] ${pkgs.coreutils}/bin/echo "$(${pkgs.coreutils}/bin/date +'%H:%M')"'';
          font_size = 120;
          font_family = "JetBrainsMono Nerd Font";
          position = "0, 500";
          halign = "center";
          valign = "center";
        }
        {
          text = ''
            cmd[update:60000] ${pkgs.coreutils}/bin/echo "$(${pkgs.coreutils}/bin/date +'%A, %B %d')"'';
          font_size = 32;
          font_family = "JetBrainsMono Nerd Font";
          position = "0, 430";
          halign = "center";
          valign = "center";
        }
        {
          text = ''
            cmd[update:43200000] ${pkgs.coreutils}/bin/echo "$(${pkgs.coreutils}/bin/date +'%Y')"'';
          font_size = 24;
          font_family = "JetBrainsMono Nerd Font";
          position = "0, 395";
          halign = "center";
          valign = "center";
        }
        {
          text = ''
            cmd[update:30000] [ -f /sys/class/power_supply/BAT0/capacity ] && ${pkgs.coreutils}/bin/echo " $(${pkgs.coreutils}/bin/cat /sys/class/power_supply/BAT0/capacity)%" || ${pkgs.coreutils}/bin/echo ""'';
          font_size = 20;
          font_family = "JetBrainsMono Nerd Font";
          position = "60, 60";
          halign = "left";
          valign = "bottom";
        }
        {
          text = ''
            cmd[update:10000] ${pkgs.iputils}/bin/ping -c 1 8.8.8.8 >/dev/null 2>&1 && ${pkgs.coreutils}/bin/echo "󰖩 Connected" || ${pkgs.coreutils}/bin/echo "󰖪 Offline"'';
          font_size = 20;
          font_family = "JetBrainsMono Nerd Font";
          position = "-60, 60";
          halign = "right";
          valign = "bottom";
        }
        {
          text = ''
            cmd[update:500] [ "$(${pkgs.coreutils}/bin/cat /sys/class/leds/*capslock/brightness)" -eq 1 ] && ${pkgs.coreutils}/bin/echo "󰪛 CAPS LOCK" || ${pkgs.coreutils}/bin/echo ""'';
          font_size = 22;
          font_family = "JetBrainsMono Nerd Font";
          position = "-60, -60";
          halign = "right";
          valign = "top";
        }
        {
          text = ''
            cmd[update:3600000] ${pkgs.coreutils}/bin/echo " $(${pkgs.hostname}/bin/hostname)"'';
          font_size = 20;
          font_family = "JetBrainsMono Nerd Font";
          position = "60, -60";
          halign = "left";
          valign = "top";
        }
        {
          text = "Enter your password to unlock";
          font_size = 18;
          font_family = "JetBrainsMono Nerd Font";
          position = "0, -150";
          halign = "center";
          valign = "center";
        }
        {
          text = ''
            cmd[update:5000] ${pkgs.coreutils}/bin/echo "󰍛 $(${pkgs.procps}/bin/uptime | ${pkgs.gawk}/bin/awk -F'load average:' '{print $2}' | ${pkgs.gawk}/bin/awk '{print $1}' | ${pkgs.coreutils}/bin/tr -d ',')"'';
          font_size = 16;
          font_family = "JetBrainsMono Nerd Font";
          position = "0, 60";
          halign = "center";
          valign = "bottom";
        }
        {
          text = ''
            cmd[update:10000] [ -f /sys/class/thermal/thermal_zone0/temp ] && ${pkgs.coreutils}/bin/echo "🌡️ $(($(${pkgs.coreutils}/bin/cat /sys/class/thermal/thermal_zone0/temp) / 1000))°C" || ${pkgs.coreutils}/bin/echo ""'';
          font_size = 16;
          font_family = "JetBrainsMono Nerd Font";
          position = "200, 60";
          halign = "left";
          valign = "bottom";
        }
        {
          text = ''
            cmd[update:5000] ${pkgs.coreutils}/bin/echo "󰘚 $(${pkgs.procps}/bin/free -h | ${pkgs.gawk}/bin/awk '/^Mem:/{print $3"/"$2}')"'';
          font_size = 16;
          font_family = "JetBrainsMono Nerd Font";
          position = "-200, 60";
          halign = "right";
          valign = "bottom";
        }
      ];
    };
  };
}
