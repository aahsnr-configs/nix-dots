{ ... }:
{
  wayland.windowManager.hyprland.settings = {
    general = {
      layout = "dwindle";
      allow_tearing = false;
      gaps_workspaces = 8;
      gaps_in = 4;
      gaps_out = 7;
      border_size = 3;
      "col.active_border" = "rgba(7aa2f7e6)"; # Tokyo Night Blue
      "col.inactive_border" = "rgba(3b4261aa)"; # Tokyo Night Dark Gray
    };

    dwindle = {
      preserve_split = true;
      smart_split = false;
      smart_resizing = false;
    };

    decoration = {
      rounding = 10;
      blur = {
        enabled = true;
        xray = false;
        special = false;
        ignore_opacity = true;
        new_optimizations = true;
        popups = true;
        input_methods = false;
        size = 3;
        passes = 4;
        contrast = 2;
      };
      # disable if costing perf
      shadow = {
        enabled = true;
        range = 10;
        render_power = 2;
        color = "rgba(1a1b26d4)"; # Tokyo Night Background
      };
    };

    animations = {
      enabled = true;
      bezier = [
        "specialWorkSwitch, 0.05, 0.7, 0.1, 1"
        "emphasizedAccel, 0.3, 0, 0.8, 0.15"
        "emphasizedDecel, 0.05, 0.7, 0.1, 1"
        "standard, 0.2, 0, 0, 1"
      ];

      animation = [
        "layersIn, 1, 4, emphasizedDecel, slide"
        "layersOut, 1, 4, emphasizedAccel, slide"
        "fadeLayers, 1, 4, standard"
        "windowsIn, 1, 4, emphasizedDecel"
        "windowsOut, 1, 4, emphasizedAccel"
        "windowsMove, 1, 4, standard"
        "workspaces, 1, 4, standard"
        "specialWorkspace, 1, 4, specialWorkSwitch, slidefadevert 15%"
        "fade, 1, 4, standard"
        "fadeDim, 1, 4, standard"
        "border, 1, 4, standard"
      ];
    };

    input = {
      kb_layout = "us";
      kb_options = "ctrl:nocaps";
      follow_mouse = 1;
      numlock_by_default = true;
      repeat_delay = 250;
      repeat_rate = 35;
      focus_on_close = 1;

      touchpad = {
        natural_scroll = true;
        disable_while_typing = true;
        scroll_factor = 0.3;
      };
    };

    gestures = {
      workspace_swipe_distance = 700;
      workspace_swipe_cancel_ratio = 0.15;
      workspace_swipe_min_speed_to_force = 5;
      workspace_swipe_direction_lock = true;
      workspace_swipe_direction_lock_threshold = 10;
      workspace_swipe_create_new = true;
    };
    gesture = [
      "4, horizontal, workspace"
      "3, up, special, special"
      "3, down, dispatcher, exec, caelestia toggle specialws"
      "4, down, dispatcher, exec, systemctl suspend-then-hibernate"
    ];

    group = {
      "col.border_active" = "rgba(7aa2f7e6)"; # Tokyo Night Blue
      "col.border_inactive" = "rgba(3b4261aa)"; # Tokyo Night Dark Gray
      "col.border_locked_active" = "rgba(7aa2f7e6)"; # Tokyo Night Blue
      "col.border_locked_inactive" = "rgba(3b4261aa)"; # Tokyo Night Dark Gray
      groupbar = {
        font_family = "JetBrainsMono Nerd Font";
        font_size = 15;
        gradients = true;
        gradient_round_only_edges = false;
        gradient_rounding = 5;
        height = 25;
        indicator_height = 0;
        gaps_in = 3;
        gaps_out = 3;

        text_color = "rgb(c0caf5)"; # Tokyo Night Foreground
        "col.active" = "rgba(7aa2f7d4)"; # Tokyo Night Blue
        "col.inactive" = "rgba(565f89d4)"; # Tokyo Night Comment
        "col.locked_active" = "rgba(7aa2f7d4)"; # Tokyo Night Blue
        "col.locked_inactive" = "rgba(bb9af7d4)"; # Tokyo Night Purple
      };
    };

    misc = {
      vfr = false;
      vrr = 2;
      animate_manual_resizes = false;
      animate_mouse_windowdragging = false;
      disable_hyprland_logo = true;
      force_default_wallpaper = 0;
      new_window_takes_over_fullscreen = 2;
      allow_session_lock_restore = true;
      middle_click_paste = false;
      focus_on_activate = true;
      session_lock_xray = true;
      mouse_move_enables_dpms = true;
      key_press_enables_dpms = true;
      font_family = "JetBrainsMono Nerd Font";
      background_color = "rgb(24283b)"; # Tokyo Night Background Highlight
      enable_swallow = true;
      swallow_regex = "^(Alacritty|kitty|footclient)$";
    };

    opengl = {
      nvidia_anti_flicker = true;
    };

    # trying if changes brave fonts issues
    # xwayland {
    #   force_zero_scaling = true
    # }

    binds = {
      scroll_event_delay = 0;
    };

    cursor = {
      hotspot_padding = 1;
    };
  };
}
