{ pkgs, ... }: {
  wayland.windowManager.hyprland = {
    settings = {
      exec-once = [ "${pkgs.hyprland}/bin/hyprctl dispatch submap global" ];
      submap = [ "global" ];
      bind = [
        #--- Caelestia App Launcher ---#
        "SUPER, D, global, ${pkgs.caelestia}/bin/caelestia:launcher"

        #--- Caelestia Misc. Actions ---#
        "CTRL_ALT, Delete, global, ${pkgs.caelestia}/bin/caelestia:session"
        "SUPER, K, global, ${pkgs.caelestia}/bin/caelestia:showall"
        "SUPER, L, global, ${pkgs.caelestia}/bin/caelestia:lock"

        #--- Workspaces & Groups ---#
        ##-- Goto --##
        # Go to Workspace
        "SUPER, 1, exec, ${pkgs.wsaction}/bin/wsaction workspace 1"
        "SUPER, 2, exec, ${pkgs.wsaction}/bin/wsaction workspace 2"
        "SUPER, 3, exec, ${pkgs.wsaction}/bin/wsaction workspace 3"
        "SUPER, 4, exec, ${pkgs.wsaction}/bin/wsaction workspace 4"
        "SUPER, 5, exec, ${pkgs.wsaction}/bin/wsaction workspace 5"
        "SUPER, 6, exec, ${pkgs.wsaction}/bin/wsaction workspace 6"
        "SUPER, 7, exec, ${pkgs.wsaction}/bin/wsaction workspace 7"
        "SUPER, 8, exec, ${pkgs.wsaction}/bin/wsaction workspace 8"
        "SUPER, 9, exec, ${pkgs.wsaction}/bin/wsaction workspace 9"
        "SUPER, 0, exec, ${pkgs.wsaction}/bin/wsaction workspace 10"

        # Go to workspace group
        "SUPERCTRL, 1, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 1"
        "SUPERCTRL, 2, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 2"
        "SUPERCTRL, 3, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 3"
        "SUPERCTRL, 4, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 4"
        "SUPERCTRL, 5, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 5"
        "SUPERCTRL, 6, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 6"
        "SUPERCTRL, 7, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 7"
        "SUPERCTRL, 8, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 8"
        "SUPERCTRL, 9, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 9"
        "SUPERCTRL, 0, exec, ${pkgs.wsaction}/bin/wsaction -g workspace 10"

        # Go to workspace group -1/+1
        "SUPERCTRL, mouse_up, workspace, +10"
        "SUPERCTRL, mouse_down, workspace, -10"

        # Toggle special workspace
        "SUPER, S, exec, ${pkgs.caelestia}/bin/caelestia toggle specialws"

        ##-- Move Window To Worskpace/Groups --##
        # Move window to workspace no.
        "SUPERALT, 1, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 1"
        "SUPERALT, 2, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 2"
        "SUPERALT, 3, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 3"
        "SUPERALT, 4, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 4"
        "SUPERALT, 5, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 5"
        "SUPERALT, 6, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 6"
        "SUPERALT, 7, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 7"
        "SUPERALT, 8, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 8"
        "SUPERALT, 9, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 9"
        "SUPERALT, 0, exec, ${pkgs.wsaction}/bin/wsaction movetoworkspace 10"

        # Move window to workspace group no.
        "SUPER_CTRL_ALT, 1, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 1"
        "SUPER_CTRL_ALT, 2, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 2"
        "SUPER_CTRL_ALT, 3, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 3"
        "SUPER_CTRL_ALT, 4, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 4"
        "SUPER_CTRL_ALT, 5, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 5"
        "SUPER_CTRL_ALT, 6, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 6"
        "SUPER_CTRL_ALT, 7, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 7"
        "SUPER_CTRL_ALT, 8, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 8"
        "SUPER_CTRL_ALT, 9, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 9"
        "SUPER_CTRL_ALT, 0, exec, ${pkgs.wsaction}/bin/wsaction -g movetoworkspace 10"

        # Move window to/from special workspace
        "SUPER_SHIFT_ALT, up, movetoworkspace, special:special"
        "SUPER_SHIFT_ALT, down, movetoworkspace, e+0"
        "SUPER_ALT, S, movetoworkspace, special:special"

        #--- Window groups ---#
        "SUPER, Comma, togglegroup"
        "SUPER, U, moveoutofgroup"
        "SUPERSHIFT, Comma, lockactivegroup, toggle"

        #--- Window Actions ---#
        "SUPER, left,  movefocus, l"
        "SUPER, right, movefocus, r"
        "SUPER, up,    movefocus, u"
        "SUPER, down,  movefocus, d"
        "SUPERSHIFT, left,  movewindow, l"
        "SUPERSHIFT, right, movewindow, r"
        "SUPERSHIFT, up,    movewindow, u"
        "SUPERSHIFT, down,  movewindow, d"
        "SUPERCTRL, left,  resizeactive, -45 0"
        "SUPERCTRL, right, resizeactive, 45 0"
        "SUPERCTRL, up,    resizeactive, 0 -45"
        "SUPERCTRL, down,  resizeactive, 0 45"
        "SUPER, P, pin"
        "SUPER, F, fullscreen, 0"
        "SUPERALT, F, fullscreen, 1" # Fullscreen with borders
        "SUPERALT, Space, togglefloating,"
        "SUPER, Q, killactive,"

        #--- Special Workspace Toggles ---#
        "CTRL_SHIFT, Escape, exec, ${pkgs.caelestia}/bin/caelestia toggle sysmon"
        "SUPER, M, exec, ${pkgs.caelestia}/bin/caelestia toggle music"
        "SUPER, C, exec, ${pkgs.caelestia}/bin/caelestia toggle communication"
        "SUPER, R, exec, ${pkgs.caelestia}/bin/caelestia toggle todo"

        #--- Applications ---#
        "SUPER, Return, exec, ${pkgs.app2unit}/bin/app2unit -- kitty"
        "SUPER, E, exec, emacsclient -c -a 'emacs'"
        "SUPER, B, exec, ${pkgs.app2unit}/bin/app2unit -- zen"
        "SUPER, Z, exec, ${pkgs.app2unit}/bin/app2unit -- zotero"
        "SUPER, T, exec, ${pkgs.app2unit}/bin/app2unit -- thunar"

        #--- Scratchpads ---#
        "SUPERSHIFT, RETURN, exec, ${pkgs.pyprland}/bin/pypr toggle term"
        "SUPERSHIFT, Y, exec, ${pkgs.pyprland}/bin/pypr toggle yazi"

        #--- Utilities ---#
        "SUPERSHIFT, S, global, ${pkgs.caelestia}/bin/caelestia:screenshotFreeze" # Capture region (freeze)
        "SUPER_SHIFT_ALT, S, global, ${pkgs.caelestia}/bin/caelestia:screenshot" # Capture region
        "SUPERALT, R, exec, ${pkgs.caelestia}/bin/caelestia record -s" # Record screen with sound
        "CTRL_ALT, R, exec, ${pkgs.caelestia}/bin/caelestia record" # Record screen
        "SUPER_SHIFT_ALT, R, exec, ${pkgs.caelestia}/bin/caelestia record -r" # Record region
        "SUPERSHIFT, C, exec, ${pkgs.hyprpicker}/bin/hyprpicker -a" # Colour picker

        #--- Sleep ---#
        "SUPERSHIFT, L, exec, systemctl suspend-then-hibernate"

        #--- Clipboard and Emoji Picker ---#
        "SUPER, V, exec, pkill ${pkgs.fuzzel}/bin/fuzzel || ${pkgs.caelestia}/bin/caelestia clipboard"
        "SUPERALT, V, exec, pkill ${pkgs.fuzzel}/bin/fuzzel || ${pkgs.caelestia}/bin/caelestia clipboard -d"
        "SUPER, Period, exec, pkill ${pkgs.fuzzel}/bin/fuzzel || ${pkgs.caelestia}/bin/caelestia emoji -p"
      ];

      # Binds that are repeatable
      binde = [
        # Go to workspace -1/+1
        "CTRL_ALT, right, workspace, +1"
        "CTRL_ALT, left, workspace, -1"

        # Move window to workspace -1/+1
        "SUPER_CTRL_ALT, right, movetoworkspace, +1"
        "SUPER_CTRL_ALT, left, movetoworkspace, -1"

        # Window Groups
        "CTRL_ALT, Tab, changegroupactive, f"
        "CTRL_SHIFT_ALT, Tab, changegroupactive, b"
        "ALT, cyclenext, activewindow"
        "ALT, cyclenext, prev, activewindow"
      ];

      # Binds that are executed on lockscreen
      bindl = [
        #--- Caelestia Misc. Actions ---#
        "CTRL_ALT, C, global, ${pkgs.caelestia}/bin/caelestia:clearNotifs"
        "SUPER_ALT, L, global, ${pkgs.caelestia}/bin/caelestia:lock"
        "SUPER_ALT, L, exec, ${pkgs.caelestia}/bin/caelestia shell -d; ${pkgs.caelestia}/bin/caelestia:lock"

        #--- Caelestia Brightness Control ---#
        ", XF86MonBrightnessUp, global, ${pkgs.caelestia}/bin/caelestia:brightnessUp"
        ", XF86MonBrightnessDown, global, ${pkgs.caelestia}/bin/caelestia:brightnessDown"

        #--- Caelestia Media Control ---#
        ", XF86AudioPlay, global, ${pkgs.caelestia}/bin/caelestia:mediaToggle"
        ", XF86AudioPause, global, ${pkgs.caelestia}/bin/caelestia:mediaToggle"
        ", XF86AudioNext, global, ${pkgs.caelestia}/bin/caelestia:mediaNext"
        ", XF86AudioPrev, global, ${pkgs.caelestia}/bin/caelestia:mediaPrev"
        ", XF86AudioStop, global, ${pkgs.caelestia}/bin/caelestia:mediaStop"

        #--- Caelestia Audio Control ---#
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

        #--- Utilities ---#
        ", Print, exec, caelestia screenshot"
        ", switch:on:lid, exec, disable-lid"
      ];

      bindle = [
        #--- Caelestia Audio Control ---#
        ", XF86AudioLowerVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%-"
        ", XF86AudioRaiseVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 10%+"
      ];

      bindr = [
        #--- Kill/Restart Caelestia Shell ---#
        "SUPER_CTRL_SHIFT, R, exec, qs -c ${pkgs.caelestia}/bin/caelestia kill"
        "SUPER_CTRL_ALT, R, exec, qs -c ${pkgs.caelestia}/bin/caelestia kill; ${pkgs.caelestia}/bin/caelestia shell -d"
      ];

      # Mouse binds
      bindm = [ "Super+Alt, mouse:272, resizewindow" ];
    };
  };
}
