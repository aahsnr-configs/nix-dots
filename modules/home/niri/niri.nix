{
  config,
  pkgs,
  inputs,
  lib,
  ...
}: let
  # Path to DMS colors file
  dmsColorsPath = "${config.home.homeDirectory}/.config/DankMaterialShell/dms-colors.json";

  # Define the DMS package for easier reference
  dms-pkg = inputs.dankMaterialShell.packages.${pkgs.system}.default;

  # A script to handle screenshots with grim, slurp, and swappy
  screenshotScript = pkgs.writeShellScriptBin "niri-screenshot" ''
    #!/usr/bin/env bash
    set -euo pipefail

    # Swappy's config will handle the save directory, but we ensure it exists.
    mkdir -p "''${HOME}/Pictures/Screenshots"

    MODE=$1

    case "$MODE" in
      full)
        # Capture the entire focused output and pipe to swappy
        ${pkgs.grim}/bin/grim - | ${pkgs.swappy}/bin/swappy -f -
        ;;
      select)
        # Select a region or window with slurp, capture it with grim, and pipe to swappy
        ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.swappy}/bin/swappy -f -
        ;;
      *)
        echo "Usage: $0 {full|select}" >&2
        exit 1
        ;;
    esac
  '';

  # Default colors (used if DMS colors file doesn't exist yet)
  defaultColors = {
    primary = "#b1c5ff";
    surfaceVariantText = "#8f9099";
    error = "#ffb4ab";
    shadow = "#000000";
  };

  # Read colors from DMS file, fallback to defaults if not found
  dmsColors =
    if builtins.pathExists dmsColorsPath
    then builtins.fromJSON (builtins.readFile dmsColorsPath)
    else defaultColors;

  # Extract colors with fallback to defaults
  colorConfig = {
    primary = dmsColors.primary or defaultColors.primary;
    inactive = dmsColors.surfaceVariantText or defaultColors.surfaceVariantText;
    urgent = dmsColors.error or defaultColors.error;
    shadow = dmsColors.shadow or defaultColors.shadow;
    # Create alpha variants
    primaryAlpha = "${dmsColors.primary or defaultColors.primary}80";
    shadowAlpha = "${dmsColors.shadow or defaultColors.shadow}70";
  };
in {
  # Import the niri-flake home-manager module for settings support
  imports = [
    inputs.niri.homeModules.niri
  ];

  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;

    settings = {
      spawn-at-startup = [
        {argv = ["niri-switch-daemon"];}
        # {argv = ["swaybg" "--image" "/path/to/wallpaper.jpg"];}
        # {argv = ["~/.config/niri/scripts/startup.sh"];}
      ];

      # Prefer no client-side decorations
      prefer-no-csd = true;

      # Disable config notifications (DMS handles this)
      config-notification.disable-failed = true;

      # Input configuration
      input = {
        keyboard = {
          xkb.layout = "us";
          track-layout = "global";
          numlock = true;
        };

        touchpad = {
          tap = true;
          natural-scroll = true;
        };

        focus-follows-mouse.enable = true;
        workspace-auto-back-and-forth = true;
      };

      # Gestures
      gestures.hot-corners.enable = true;

      # Output configuration
      outputs = {
        "HDMI-A-1" = {
          mode = {
            width = 2560;
            height = 1440;
            refresh = 60.0;
          };
          scale = 2.0;
        };
        "eDP-1".enable = false;
      };

      # Hotkey overlay
      hotkey-overlay.skip-at-startup = true;

      # Layout configuration
      layout = {
        gaps = 10;
        center-focused-column = "never";

        preset-column-widths = [
          {proportion = 0.33333;}
          {proportion = 0.5;}
          {proportion = 0.66667;}
          {proportion = 1.0;}
        ];

        default-column-width = {
          #proportion = 0.4;
        };

        # Border configuration (disabled, using focus-ring instead)
        border.enable = false;

        # Focus ring with DYNAMIC colors from DMS
        focus-ring = {
          width = 2;
          enable = true;
          active.color = colorConfig.primary;
          inactive.color = colorConfig.inactive;
          urgent.color = colorConfig.urgent;
        };

        shadow = {
          enable = true;
          color = colorConfig.shadowAlpha;
          softness = 30.0;
          spread = 5.0;
          offset = {
            x = 0;
            y = 5;
          };
        };

        # Tab indicator colors for tabbed display (DYNAMIC)
        tab-indicator = {
          enable = true;
          width = 4.0;
          gap = 5.0;
          position = "left";
          corner-radius = 0.0;
          gaps-between-tabs = 0.0;
          hide-when-single-tab = false;
          place-within-column = false;
          length.total-proportion = 0.5;
          active.color = colorConfig.primary;
          inactive.color = colorConfig.inactive;
          urgent.color = colorConfig.urgent;
        };

        # Insert hint color for interactive window moves (DYNAMIC)
        insert-hint = {
          enable = true;
          display.color = colorConfig.primaryAlpha;
        };

        # Background color (transparent as per colors.kdl)
        background-color = null;

        struts = {};
      };

      # Overview settings with shadow colors (DYNAMIC)
      overview = {
        workspace-shadow = {
          enable = true;
          color = colorConfig.shadowAlpha;
        };
        backdrop-color = null;
      };

      # Window rules - applying to all windows by default
      window-rules = [
        {
          geometry-corner-radius = {
            top-left = 10.0;
            top-right = 10.0;
            bottom-left = 10.0;
            bottom-right = 10.0;
          };
          clip-to-geometry = true;
          draw-border-with-background = false;
        }
        {
          matches = [{app-id = "^org\\.wezfurlong\\.wezterm$";}];
          default-column-width = {};
        }
        {
          matches = [{app-id = "^org\\.gnome\\.";}];
          draw-border-with-background = false;
          geometry-corner-radius = {
            top-left = 12.0;
            top-right = 12.0;
            bottom-left = 12.0;
            bottom-right = 12.0;
          };
          clip-to-geometry = true;
        }
        {
          matches = [
            {app-id = "^gnome-control-center$";}
            {app-id = "^pavucontrol$";}
            {app-id = "^nm-connection-editor$";}
          ];
          default-column-width = {
            proportion = 0.5;
          };
          open-floating = false;
        }
        {
          matches = [
            {app-id = "^gnome-calculator$";}
            {app-id = "^galculator$";}
            {app-id = "^blueman-manager$";}
            {app-id = "^org\\.gnome\\.Nautilus$";}
            {app-id = "^steam$";}
            {app-id = "^xdg-desktop-portal$";}
          ];
          open-floating = true;
        }
        {
          matches = [
            {app-id = "^org\\.wezfurlong\\.wezterm$";}
            {app-id = "Alacritty";}
            {app-id = "zen";}
            {app-id = "com.mitchellh.ghostty";}
            {app-id = "kitty";}
          ];
          draw-border-with-background = false;
        }
        {
          matches = [{is-active = false;}];
          opacity = 1.0;
        }
        {
          matches = [
            {
              app-id = "zen$";
              title = "^Picture-in-Picture$";
            }
            {app-id = "zoom";}
          ];
          open-floating = true;
        }
      ];

      # Layer rules for DMS quickshell and wallpaper blur
      layer-rules = [
        {
          matches = [{namespace = "^quickshell$";}];
          place-within-backdrop = true;
        }
        {
          matches = [{namespace = "dms:blurwallpaper";}];
          place-within-backdrop = true;
        }
      ];

      # Environment variables
      environment = {
        XDG_CURRENT_DESKTOP = "niri";
        QT_QPA_PLATFORM = "wayland";
        ELECTRON_OZONE_PLATFORM_HINT = "auto";
        QT_QPA_PLATFORMTHEME = "gtk3";
        QT_QPA_PLATFORMTHEME_QT6 = "gtk3";
        TERMINAL = "kitty";
      };

      # Animations
      animations = {
        workspace-switch.kind = {
          spring = {
            damping-ratio = 0.80;
            stiffness = 523;
            epsilon = 0.0001;
          };
        };

        window-open.kind = {
          easing = {
            duration-ms = 300;
            curve = "ease-out-expo";
          };
        };

        window-close.kind = {
          easing = {
            duration-ms = 300;
            curve = "ease-out-quad";
          };
        };

        horizontal-view-movement.kind = {
          spring = {
            damping-ratio = 0.85;
            stiffness = 423;
            epsilon = 0.0001;
          };
        };

        window-movement.kind = {
          spring = {
            damping-ratio = 0.75;
            stiffness = 323;
            epsilon = 0.0001;
          };
        };

        window-resize.kind = {
          spring = {
            damping-ratio = 0.85;
            stiffness = 423;
            epsilon = 0.0001;
          };
        };

        config-notification-open-close.kind = {
          spring = {
            damping-ratio = 0.65;
            stiffness = 923;
            epsilon = 0.001;
          };
        };

        overview-open-close.kind = {
          spring = {
            damping-ratio = 0.85;
            stiffness = 800;
            epsilon = 0.0001;
          };
        };
      };

      # Keybindings - DMS handles its own bindings via IPC
      # These are compositor-level window management bindings only
      binds = with config.lib.niri.actions; {
        # System & Overview
        "Mod+Tab".action = toggle-overview;
        "Mod+Shift+Slash".action = show-hotkey-overlay;

        # DMS Application Launchers and Controls
        "Mod+D" = {
          action = spawn "dms" "ipc" "call" "spotlight" "toggle";
          hotkey-overlay.title = "Application Launcher";
        };
        "Mod+V" = {
          action = spawn "dms" "ipc" "call" "clipboard" "toggle";
          hotkey-overlay.title = "Clipboard Manager";
        };
        "Mod+M" = {
          action = spawn "dms" "ipc" "call" "processlist" "toggle";
          hotkey-overlay.title = "Task Manager";
        };
        "Mod+Comma" = {
          action = spawn "dms" "ipc" "call" "settings" "toggle";
          hotkey-overlay.title = "Settings";
        };
        "Mod+N" = {
          action = spawn "dms" "ipc" "call" "notifications" "toggle";
          hotkey-overlay.title = "Notification Center";
        };
        "Mod+Y" = {
          action = spawn "dms" "ipc" "call" "dankdash" "wallpaper";
          hotkey-overlay.title = "Browse Wallpapers";
        };
        "Mod+Shift+N" = {
          action = spawn "dms" "ipc" "call" "notepad" "toggle";
          hotkey-overlay.title = "Notepad";
        };
        "Mod+Alt+L" = {
          action = spawn "dms" "ipc" "call" "lock" "lock";
          hotkey-overlay.title = "Lock Screen";
        };
        "Ctrl+Alt+Delete" = {
          action = spawn "dms" "ipc" "call" "processlist" "toggle";
          hotkey-overlay.title = "Task Manager";
        };

        # System Application Launchers
        "Mod+Return".action = spawn "kitty";
        "Mod+E".action = spawn "emacsclient" "-c" "-a" "'emacs'";
        "Mod+Shift+E".action = spawn "nautilus";

        # Window Management
        "Mod+Q".action = close-window;
        "Mod+F".action = maximize-column;
        "Mod+Shift+F".action = fullscreen-window;
        "Mod+Shift+T".action = toggle-window-floating;
        "Mod+Shift+V".action = switch-focus-between-floating-and-tiling;
        "Mod+W".action = toggle-column-tabbed-display;

        # Focus Navigation
        "Mod+Left".action = focus-column-left;
        "Mod+Down".action = focus-window-down;
        "Mod+Up".action = focus-window-up;
        "Mod+Right".action = focus-column-right;

        # Window Movement
        "Mod+Shift+Left".action = move-column-left;
        "Mod+Shift+Down".action = move-window-down;
        "Mod+Shift+Up".action = move-window-up;
        "Mod+Shift+Right".action = move-column-right;

        # Column Navigation
        "Mod+Home".action = focus-column-first;
        "Mod+End".action = focus-column-last;
        "Mod+Ctrl+Home".action = move-column-to-first;
        "Mod+Ctrl+End".action = move-column-to-last;

        # Monitor Navigation
        "Mod+Ctrl+Left".action = focus-monitor-left;
        "Mod+Ctrl+Right".action = focus-monitor-right;
        "Mod+Ctrl+H".action = focus-monitor-left;
        "Mod+Ctrl+J".action = focus-monitor-down;
        "Mod+Ctrl+K".action = focus-monitor-up;
        "Mod+Ctrl+L".action = focus-monitor-right;

        # Move to Monitor
        "Mod+Shift+Ctrl+Left".action = move-column-to-monitor-left;
        "Mod+Shift+Ctrl+Down".action = move-column-to-monitor-down;
        "Mod+Shift+Ctrl+Up".action = move-column-to-monitor-up;
        "Mod+Shift+Ctrl+Right".action = move-column-to-monitor-right;
        "Mod+Shift+Ctrl+H".action = move-column-to-monitor-left;
        "Mod+Shift+Ctrl+J".action = move-column-to-monitor-down;
        "Mod+Shift+Ctrl+K".action = move-column-to-monitor-up;
        "Mod+Shift+Ctrl+L".action = move-column-to-monitor-right;

        # Workspace Navigation
        "Mod+Page_Down".action = focus-workspace-down;
        "Mod+Page_Up".action = focus-workspace-up;
        "Mod+U".action = focus-workspace-down;
        "Mod+I".action = focus-workspace-up;
        "Mod+Ctrl+Down".action = move-column-to-workspace-down;
        "Mod+Ctrl+Up".action = move-column-to-workspace-up;
        "Mod+Ctrl+U".action = move-column-to-workspace-down;
        "Mod+Ctrl+I".action = move-column-to-workspace-up;

        # Move Workspaces
        "Mod+Shift+Page_Down".action = move-workspace-down;
        "Mod+Shift+Page_Up".action = move-workspace-up;
        "Mod+Shift+U".action = move-workspace-down;
        "Mod+Shift+I".action = move-workspace-up;

        # Mouse Wheel Navigation
        "Mod+WheelScrollDown" = {
          action = focus-workspace-down;
          cooldown-ms = 150;
        };
        "Mod+WheelScrollUp" = {
          action = focus-workspace-up;
          cooldown-ms = 150;
        };
        "Mod+Ctrl+WheelScrollDown" = {
          action = move-column-to-workspace-down;
          cooldown-ms = 150;
        };
        "Mod+Ctrl+WheelScrollUp" = {
          action = move-column-to-workspace-up;
          cooldown-ms = 150;
        };
        "Mod+WheelScrollRight".action = focus-column-right;
        "Mod+WheelScrollLeft".action = focus-column-left;
        "Mod+Ctrl+WheelScrollRight".action = move-column-right;
        "Mod+Ctrl+WheelScrollLeft".action = move-column-left;
        "Mod+Shift+WheelScrollDown".action = focus-column-right;
        "Mod+Shift+WheelScrollUp".action = focus-column-left;
        "Mod+Ctrl+Shift+WheelScrollDown".action = move-column-right;
        "Mod+Ctrl+Shift+WheelScrollUp".action = move-column-left;

        # Numbered Workspaces
        "Mod+1".action = focus-workspace 1;
        "Mod+2".action = focus-workspace 2;
        "Mod+3".action = focus-workspace 3;
        "Mod+4".action = focus-workspace 4;
        "Mod+5".action = focus-workspace 5;
        "Mod+6".action = focus-workspace 6;
        "Mod+7".action = focus-workspace 7;
        "Mod+8".action = focus-workspace 8;
        "Mod+9".action = focus-workspace 9;

        # Move to Numbered Workspaces
        "Mod+Shift+1".action = {"move-column-to-workspace" = 1;};
        "Mod+Shift+2".action = {"move-column-to-workspace" = 2;};
        "Mod+Shift+3".action = {"move-column-to-workspace" = 3;};
        "Mod+Shift+4".action = {"move-column-to-workspace" = 4;};
        "Mod+Shift+5".action = {"move-column-to-workspace" = 5;};
        "Mod+Shift+6".action = {"move-column-to-workspace" = 6;};
        "Mod+Shift+7".action = {"move-column-to-workspace" = 7;};
        "Mod+Shift+8".action = {"move-column-to-workspace" = 8;};
        "Mod+Shift+9".action = {"move-column-to-workspace" = 9;};

        # Column Management
        "Mod+BracketLeft".action = consume-or-expel-window-left;
        "Mod+BracketRight".action = consume-or-expel-window-right;
        "Mod+Period".action = expel-window-from-column;

        # Sizing & Layout
        "Mod+R".action = switch-preset-column-width;
        "Mod+Shift+R".action = switch-preset-window-height;
        "Mod+Ctrl+R".action = reset-window-height;
        "Mod+Ctrl+F".action = expand-column-to-available-width;
        "Mod+C".action = center-column;
        "Mod+Ctrl+C".action = center-visible-columns;

        # Manual Sizing
        "Mod+Minus".action = set-column-width "-10%";
        "Mod+Equal".action = set-column-width "+10%";
        "Mod+Shift+Minus".action = set-window-height "-10%";
        "Mod+Shift+Equal".action = set-window-height "+10%";

        # Screenshots (using grim, slurp, and swappy)
        "Ctrl+Print".action = spawn "${screenshotScript}/bin/niri-screenshot" "full";
        "Mod+Shift+S".action = spawn "${screenshotScript}/bin/niri-screenshot" "select";
        "Alt+Print".action = spawn "${screenshotScript}/bin/niri-screenshot" "select";

        # Audio Controls
        "XF86AudioRaiseVolume" = {
          action = spawn "dms" "ipc" "call" "audio" "increment" "3";
          allow-when-locked = true;
        };
        "XF86AudioLowerVolume" = {
          action = spawn "dms" "ipc" "call" "audio" "decrement" "3";
          allow-when-locked = true;
        };
        "XF86AudioMute" = {
          action = spawn "dms" "ipc" "call" "audio" "mute";
          allow-when-locked = true;
        };
        "XF86AudioMicMute" = {
          action = spawn "dms" "ipc" "call" "audio" "micmute";
          allow-when-locked = true;
        };

        # Brightness Controls
        "XF86MonBrightnessUp" = {
          action = spawn "dms" "ipc" "call" "brightness" "increment" "5" "";
          allow-when-locked = true;
        };
        "XF86MonBrightnessDown" = {
          action = spawn "dms" "ipc" "call" "brightness" "decrement" "5" "";
          allow-when-locked = true;
        };

        # System Controls
        "Mod+Escape" = {
          action = toggle-keyboard-shortcuts-inhibit;
          allow-inhibiting = false;
        };
        "Mod+Shift+P".action = power-off-monitors;

        "Alt+Tab".action = spawn "niri-switch";
      };
    };
  };

  # Add packages required by niri and its ecosystem
  home.packages = with pkgs; [
    wayland-utils
    cage
    xwayland-satellite-unstable
    nautilus
    file-roller
    grim
    slurp
    swappy
    screenshotScript
    inputs.niri-switch.packages.${system}.default
  ];
}
