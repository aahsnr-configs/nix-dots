{ inputs, ... }: {
  imports = [ inputs.caelestia-shell.homeManagerModules.default ];

  programs.caelestia = {
    enable = true;
    systemd = {
      enable = true;
      target = "graphical-session.target";
    };

    settings = {
      appearance = {
        font.family = {
          material = "Material Symbols Rounder";
          mono = "JetBrainMono Nerd Font";
          sans = "Rubik";
        };
      };

      background.desktopClock = false;

      bar = {
        persistent = true;
        workspaces.shown = 10;
      };

      cli = {
        enable = true;
        settings = { theme.enableGtk = false; };
      };

      environment = [ ];

      general = {
        apps = {
          terminal = "foot";
          audio = "pavucontrol";
        };
      };

      paths = {
        mediaGif = "./assets/bongocat.git";
        sessionGif = "./assets/kurukuru.gif";
        wallpaperDir = "./wallpapers";
      };

      services = {
        gpuType = ""; # not need to be set for Wayland
        weatherLocation = "Dhaka";
        useTwelveHourClock = true;
      };

      launcher = {
        maxShown = 10;
        useFuzzy.apps = true;
      };
    };
  };
}
