{ inputs, pkgs, ... }: {
  home.packages = with pkgs; [
    app2unit
    aubio
    brightnessctl
    cava
    dart-sass
    ddcutil
    fish
    fuzzel
    grim
    libcalculate
    libnotify
    libpulseaudio
    lm_sensors
    qt6
    quickshell.packages.x86_64-linux.default
    slurp
    swappy
    wf-recorder
    wl-screenrec
    xkeyboard-config
  ];

  imports = [ inputs.caelestia-shell.homeManagerModules.default ];

  programs.caelestia = {
    enable = true;
    cli = {
      enable = true;
      settings = {
        theme = {
          enableTerm = true;
          enableHypr = true;
          enableDiscord = true;
          enableSpicetify = true;
          enableFuzzel = true;
          enableBtop = true;
          enableGtk = true;
          enableQt = true;
        };
        toggles = {
          communication = {
            discord = {
              enable = true;
              match = [{ class = "discord"; }];
              command = [ "discord" ];
              move = true;
            };
            whatsapp = {
              enable = true;
              match = [{ class = "whatsapp"; }];
              move = true;
            };
          };
          music = {
            spotify = {
              enable = true;
              match = [
                { class = "Spotify"; }
                { initialTitle = "Spotify"; }
                { initialTitle = "Spotify Free"; }
              ];
              command = [ "spicetify" "watch" "-s" ];
              move = true;
            };
            feishin = {
              enable = true;
              match = [{ class = "feishin"; }];
              move = true;
            };
          };
          sysmon = {
            btop = {
              enable = true;
              match = [{
                class = "btop";
                title = "btop";
                workspace = { name = "special:sysmon"; };
              }];
              command =
                [ "foot" "-a" "btop" "-T" "btop" "fish" "-C" "exec btop" ];
            };
          };
          todo = {
            todoist = {
              enable = true;
              match = [{ class = "Todoist"; }];
              command = [ "todoist" ];
              move = true;
            };
          };
        };
      };
    };
    settings = {
      appearance = {
        anim.durations.scale = 1;
        font = {
          family = {
            material = "Material Symbols Rounded";
            mono = "JetBrainsMono Nerd Font";
            sans = "Rubik Medium";
          };
          size.scale = 1;
        };
        padding.scale = 1;
        rounding.scale = 1;
        spacing.scale = 1;
        transparency = {
          enabled = false;
          base = 0.85;
          layers = 0.4;
        };
      };
      general = {
        apps = {
          terminal = [ "kitty" ];
          audio = [ "pavucontrol" ];
        };
      };
      background = {
        desktopClock.enabled = true;
        enabled = true;
        visualiser = {
          enabled = false;
          autoHide = true;
          rounding = 1;
          spacing = 1;
        };
      };
      bar = {
        clock.showIcon = true;
        dragThreshold = 20;
        entries = [
          {
            id = "logo";
            enabled = true;
          }
          {
            id = "workspaces";
            enabled = true;
          }
          {
            id = "spacer";
            enabled = true;
          }
          {
            id = "activeWindow";
            enabled = true;
          }
          {
            id = "spacer";
            enabled = true;
          }
          {
            id = "tray";
            enabled = true;
          }
          {
            id = "clock";
            enabled = true;
          }
          {
            id = "statusIcons";
            enabled = true;
          }
          {
            id = "power";
            enabled = true;
          }
          {
            id = "idleInhibitor";
            enabled = false;
          }
        ];
        persistent = true;
        showOnHover = true;
        status = {
          showAudio = false;
          showBattery = true;
          showBluetooth = true;
          showKbLayout = false;
          showNetwork = true;
          showLockStatus = true;
        };
        tray = {
          background = false;
          recolour = false;
        };
        workspaces = {
          activeIndicator = true;
          activeLabel = "󰮯";
          activeTrail = false;
          label = "  ";
          occupiedBg = false;
          occupiedLabel = "󰮯";
          perMonitorWorkspaces = true;
          showWindows = true;
          shown = 10;
        };
      };
      border = {
        rounding = 25;
        thickness = 10;
      };
      dashboard = {
        enabled = true;
        dragThreshold = 50;
        mediaUpdateInterval = 500;
        showOnHover = true;
      };
      launcher = {
        actionPrefix = ">";
        dragThreshold = 50;
        vimKeybinds = false;
        enableDangerousActions = false;
        maxShown = 8;
        maxWallpapers = 9;
        specialPrefix = "@";
        useFuzzy = {
          apps = false;
          actions = false;
          schemes = false;
          variants = false;
          wallpapers = false;
        };
        showOnHover = false;
      };
      lock = { recolourLogo = false; };
      notifs = {
        actionOnClick = false;
        clearThreshold = 0.3;
        defaultExpireTimeout = 5000;
        expandThreshold = 20;
        expire = false;
      };
      osd = {
        enabled = true;
        enableBrightness = true;
        enableMicrophone = false;
        hideDelay = 2000;
      };
      paths = {
        mediaGif = "./assets/bongocat.gif";
        sessionGif = "./assets/kurukuru.gif";
        wallpaperDir = "./assets/wallpapers";
      };
      services = {
        audioIncrement = 0.1;
        defaultPlayer = "Spotify";
        gpuType = "";
        playerAliases = [{
          from = "com.github.th_ch.youtube_music";
          to = "YT Music";
        }];
        weatherLocation = "";
        useFahrenheit = false;
        useTwelveHourClock = false;
        smartScheme = true;
        visualiserBars = 45;
      };
      session = {
        dragThreshold = 30;
        vimKeybinds = false;
        commands = {
          logout = [ "loginctl" "terminate-user" "" ];
          shutdown = [ "systemctl" "poweroff" ];
          hibernate = [ "systemctl" "hibernate" ];
          reboot = [ "systemctl" "reboot" ];
        };
      };
    };
  };
}
