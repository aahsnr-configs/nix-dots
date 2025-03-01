{ config, inputs, pkgs, ... }:

{
  wayland.windowManager.hyprland.settings = {
    general = {
      gaps_in = 7;
      gaps_out = 12;
      border_size = 3;
      "col.active_border" = "rgb(F0C6C6)";
      "col.inactive_border" = "rgb(181926)";
      layout = "hy3";
      hover_icon_on_border = true;
    };

    decoration = {
      rounding = 14;
      active_opacity = 1.0;
      inactive_opacity = 1.0;
      fullscreen_opacity = 1.0;
      blur = {
        enabled = true;
        passes = 4;
        new_optimizations = true;
        ignore_opacity = false;
      };
      dim_inactive = false;
      dim_strength = 1.0;
    };

    animations = {
      enabled = "yes";
      bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
      animation = [
        "windows, 1, 4, myBezier"
        "windowsOut, 1, 4, default, popin 80%"
        "border, 1, 4, default"
        "borderangle, 1, 4, default"
        "fade, 1, 4, default"
        "workspaces, 1, 4, default"
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
      workspace_swipe = true;
      workspace_swipe_fingers = 3;
      workspace_swipe_distance = 300;
      workspace_swipe_invert = true;
      workspace_swipe_min_speed_to_force = 30;
      workspace_swipe_cancel_ratio = 0.5;
      workspace_swipe_create_new = true;
      workspace_swipe_forever = true;
    };

    dwindle = {
      pseudotile = false;
      force_split = 0;
      preserve_split = true;
      special_scale_factor = 0.97;
      split_width_multiplier = 1.0;
      use_active_for_splits = true;
      default_split_ratio = 1;
    };

    master = {
      allow_small_split	= false;
      special_scale_factor = 0.97;
      mfact	= 0.55;
      inherit_fullscreen = true;
    };

    misc = {
      disable_hyprland_logo = true;
      disable_splash_rendering = false;
      mouse_move_enables_dpms = false;
      always_follow_on_dnd = true;
      layers_hog_keyboard_focus = true;
      animate_manual_resizes = true;
      animate_mouse_windowdragging = true;
      disable_autoreload = false;
      focus_on_activate = true;
      enable_swallow = true;
      swallow_regex = "^(Alacritty|kitty|footclient)$";
      vfr = true;
      vrr = true;
    };

    experimental = {
      #hdr = true;
      #xx_color_management_v4 = true;
    };
  };
}
