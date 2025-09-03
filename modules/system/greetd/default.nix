{
  pkgs,
  lib,
  ...
}: {
  programs.regreet = {
    enable = true;
    package = pkgs.greetd.regreet;
    theme.name = "Colloid-Orange-Dark-Catppuccin";
    font = {
      name = "JetBrainsMono Nerd Font Medium";
      size = 24;
    };
    iconTheme.name = "Papirus-Dark";
    cursorTheme.name = "Bibata-Modern-Ice";
    settings = {
      background = {
        path = "./wallpapers/wall1.jpg";
        fit = "Contain"; # Available values: "Fill", "Contain", "Cover", "ScaleDown"
      };

      GTK = {application_prefer_dark_theme = true;};

      commands = {
        reboot = ["systemctl" "reboot"];
        poweroff = ["systemctl" "poweroff"];
        x11_prefix = ["startx" "/usr/bin/env"];
      };

      appearance = {greeting_msg = "Welcome back!";};

      widget = {
        clock = {
          format = "%a %H:%M";
          resolution = "500ms";
          timezone = lib.mkDefault "Asia/Dhaka";
          label_width = 150;
        };
      };
    };
  };

  environment.etc."greetd/hyprland.conf".text = ''
    exec-once = regreet; hyprctl dispatch exit
    misc {
      disable_hyprland_logo = true
      disable_splash_rendering = true
      disable_hyprland_qtutils_check = true
    }
  '';

  services = {
    greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          command = "Hyprland --config /etc/greetd/hyprland.conf";
          user = "greeter";
        };
        default_session = initial_session;
        terminal.vt = 1;
      };
    };
  };
}
