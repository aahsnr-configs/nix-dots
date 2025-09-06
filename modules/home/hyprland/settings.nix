{...}: {
  wayland.windowManager.hyprland.settings = {
    general = {
      layout = "dwindle";
      allow_tearing = false;
      gaps_workspaces = 12;
      gaps_in = 7;
      gaps_out = 12;
      border_size = 3;
      "col.active_border" = "rgb(dd7878)";
      "col.inactive_border" = "rgb(181926)";
      hover_icon_on_border = true;
    };

    dwindle = {
      preserve_split = true;
      smart_split = false;
      smart_resizing = true;
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
        input_methods = true;
        size = 8;
        passes = 2;
      };
      shadow = {
        enabled = true;
        range = 20;
        render_power = 3;
        color = "rgba(0000002A)";
      };
    };

    animations = {
      enabled = true;
      bezier = [
        "emphasizedAccel, 0.3, 0, 0.8, 0.15"
        "emphasizedDecel, 0.05, 0.7, 0.1, 1"
        "specialWorkSwitch, 0.05, 0.7, 0.1, 1"
        "standard, 0.2, 0, 0, 1"
      ];
      animation = [
        "border, 1, 6, standard"
        "fade, 1, 6, standard"
        "fadeDim, 1, 6, standard"
        "fadeLayers, 1, 5, standard"
        "layersIn, 1, 5, emphasizedDecel, slide"
        "layersOut, 1, 4, emphasizedAccel, slide"
        "specialWorkspace, 1, 4, specialWorkSwitch, slidefadevert 15%"
        "windowsIn, 1, 5, emphasizedDecel"
        "windowsMove, 1, 6, standard"
        "windowsOut, 1, 3, emphasizedAccel"
        "workspaces, 1, 5, standard"
      ];
    };

    input = {
      kb_layout = "us";
      kb_options = "ctrl:nocaps";
      follow_mouse = 1;
      touchpad = {
        natural_scroll = "yes";
        disable_while_typing = true;
        drag_lock = true;
      };
      force_no_accel = 0;
      sensitivity = 0;
      natural_scroll = 0;
      float_switch_override_focus = 2;
      numlock_by_default = true;
    };

    gestures = {
      workspace_swipe_distance = 300;
      workspace_swipe_invert = true;
      workspace_swipe_min_speed_to_force = 30;
      workspace_swipe_cancel_ratio = 0.5;
      workspace_swipe_create_new = true;
      workspace_swipe_forever = true;
    };

    master = {
      allow_small_split = false;
      special_scale_factor = 0.97;
      mfact = 0.55;
      inherit_fullscreen = true;
    };

    misc = {
      animate_manual_resizes = true;
      animate_mouse_windowdragging = true;
      always_follow_on_dnd = true;
      disable_autoreload = false;
      disable_hyprland_logo = true;
      disable_splash_rendering = false;
      enable_anr_dialog = true;
      enable_swallow = true;
      focus_on_activate = true;
      font_family = "JetBrainsMono Nerd Font";
      layers_hog_keyboard_focus = true;
      mouse_move_enables_dpms = false;
      swallow_regex = "^(Alacritty|kitty|footclient)$";
      vfr = true;
      vrr = true;
    };
  };
}
