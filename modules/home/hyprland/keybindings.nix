{...}: {
  wayland.windowManager.hyprland = {
    settings = {
      exec-once = ["hyprctl dispatch submap global"];
      submap = ["global"];
      bind = [
        #--- Caelestia App Launcher ---#
        "SUPER, D, global, caelestia:launcher"

        #--- Caelestia Misc. Actions ---#
        "CTRL_ALT, Delete, global, caelestia:session"
        "SUPER, K, global, caelestia:showall"
        "SUPER, L, global, caelestia:lock"

        #--- Workspaces & Groups ---#
        ##-- Goto --##
        # Go to Workspace
        "SUPER, 1, exec, wsaction workspace 1"
        "SUPER, 2, exec, wsaction workspace 2"
        "SUPER, 3, exec, wsaction workspace 3"
        "SUPER, 4, exec, wsaction workspace 4"
        "SUPER, 5, exec, wsaction workspace 5"
        "SUPER, 6, exec, wsaction workspace 6"
        "SUPER, 7, exec, wsaction workspace 7"
        "SUPER, 8, exec, wsaction workspace 8"
        "SUPER, 9, exec, wsaction workspace 9"
        "SUPER, 0, exec, wsaction workspace 10"

        # Go to workspace group
        "SUPER_CTRL, 1, exec, wsaction -g workspace 1"
        "SUPER_CTRL, 2, exec, wsaction -g workspace 2"
        "SUPER_CTRL, 3, exec, wsaction -g workspace 3"
        "SUPER_CTRL, 4, exec, wsaction -g workspace 4"
        "SUPER_CTRL, 5, exec, wsaction -g workspace 5"
        "SUPER_CTRL, 6, exec, wsaction -g workspace 6"
        "SUPER_CTRL, 7, exec, wsaction -g workspace 7"
        "SUPER_CTRL, 8, exec, wsaction -g workspace 8"
        "SUPER_CTRL, 9, exec, wsaction -g workspace 9"
        "SUPER_CTRL, 0, exec, wsaction -g workspace 10"

        # Go to workspace group -1/+1
        "SUPER_CTRL, mouse_up, workspace, +10"
        "SUPER_CTRL, mouse_down, workspace, -10"

        # Toggle special workspace
        "SUPER, S, exec, caelestia toggle specialws"

        ##-- Move Window To Worskpace/Groups --##
        # Move window to workspace no.
        "SUPER_ALT, 1, exec, wsaction movetoworkspace 1"
        "SUPER_ALT, 2, exec, wsaction movetoworkspace 2"
        "SUPER_ALT, 3, exec, wsaction movetoworkspace 3"
        "SUPER_ALT, 4, exec, wsaction movetoworkspace 4"
        "SUPER_ALT, 5, exec, wsaction movetoworkspace 5"
        "SUPER_ALT, 6, exec, wsaction movetoworkspace 6"
        "SUPER_ALT, 7, exec, wsaction movetoworkspace 7"
        "SUPER_ALT, 8, exec, wsaction movetoworkspace 8"
        "SUPER_ALT, 9, exec, wsaction movetoworkspace 9"
        "SUPER_ALT, 0, exec, wsaction movetoworkspace 10"

        # Move window to workspace group no.
        "SUPER_CTRL_ALT, 1, exec, wsaction -g movetoworkspace 1"
        "SUPER_CTRL_ALT, 2, exec, wsaction -g movetoworkspace 2"
        "SUPER_CTRL_ALT, 3, exec, wsaction -g movetoworkspace 3"
        "SUPER_CTRL_ALT, 4, exec, wsaction -g movetoworkspace 4"
        "SUPER_CTRL_ALT, 5, exec, wsaction -g movetoworkspace 5"
        "SUPER_CTRL_ALT, 6, exec, wsaction -g movetoworkspace 6"
        "SUPER_CTRL_ALT, 7, exec, wsaction -g movetoworkspace 7"
        "SUPER_CTRL_ALT, 8, exec, wsaction -g movetoworkspace 8"
        "SUPER_CTRL_ALT, 9, exec, wsaction -g movetoworkspace 9"
        "SUPER_CTRL_ALT, 0, exec, wsaction -g movetoworkspace 10"

        # Move window to/from special workspace
        "SUPER_SHIFT_ALT, up, movetoworkspace, special:special"
        "SUPER_SHIFT_ALT, down, movetoworkspace, e+0"
        "SUPER_ALT, S, movetoworkspace, special:special"

        #--- Window groups ---#
        "SUPER, Comma, togglegroup"
        "SUPER, U, moveoutofgroup"
        "SUPER_SHIFT, Comma, lockactivegroup, toggle"

        #--- Window Actions ---#
        "SUPER, left,  movefocus, l"
        "SUPER, right, movefocus, r"
        "SUPER, up,    movefocus, u"
        "SUPER, down,  movefocus, d"
        "SUPER_SHIFT, left,  movewindow, l"
        "SUPER_SHIFT, right, movewindow, r"
        "SUPER_SHIFT, up,    movewindow, u"
        "SUPER_SHIFT, down,  movewindow, d"
        "SUPER_CTRL, left,  resizeactive, -45 0"
        "SUPER_CTRL, right, resizeactive, 45 0"
        "SUPER_CTRL, up,    resizeactive, 0 -45"
        "SUPER_CTRL, down,  resizeactive, 0 45"
        "SUPER, P, pin"
        "SUPER, F, fullscreen, 0"
        "SUPER_ALT, F, fullscreen, 1" # Fullscreen with borders
        "SUPER_ALT, Space, togglefloating,"
        "SUPER, Q, killactive,"

        #--- Special Workspace Toggles ---#
        "CTRL_SHIFT, Escape, exec, caelestia toggle sysmon"
        "SUPER, M, exec, caelestia toggle music"
        "SUPER, C, exec, caelestia toggle communication"
        "SUPER, R, exec, caelestia toggle todo"

        #--- Applications ---#
        "SUPER, Return, exec, app2unit -- kitty"
        "SUPER, E, exec, emacsclient -c -a 'emacs'"
        "SUPER, B, exec, app2unit -- zen"
        "SUPER, Z, exec, app2unit -- zotero"
        "SUPER, T, exec, app2unit -- thunar"

        #--- Scratchpads ---#
        "SUPER_SHIFT, RETURN, exec, pypr toggle term"
        "SUPER_SHIFT, Y, exec, pypr toggle yazi"

        #--- Utilities ---#
        "SUPER_SHIFT, S, global, caelestia:screenshotFreeze" # Capture region (freeze)
        "SUPER_SHIFT_ALT, S, global, caelestia:screenshot" # Capture region
        "SUPER_ALT, R, exec, caelestia record -s" # Record screen with sound
        "CTRL_ALT, R, exec, caelestia record" # Record screen
        "SUPER_SHIFT_ALT, R, exec, caelestia record -r" # Record region
        "SUPER_SHIFT, C, exec, hyprpicker -a" # Colour picker

        #--- Sleep ---#
        "SUPER_SHIFT, L, exec, systemctl suspend-then-hibernate"

        #--- Clipboard and Emoji Picker ---#
        "SUPER, V, exec, pkill fuzzel || caelestia clipboard"
        "SUPER_ALT, V, exec, pkill fuzzel || caelestia clipboard -d"
        "SUPER, Period, exec, pkill fuzzel || caelestia emoji -p"
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
        "CTRL_ALT, C, global, caelestia:clearNotifs"
        "SUPER_ALT, L, global, caelestia:lock"
        "SUPER_ALT, L, exec, caelestia shell -d; caelestia:lock"

        #--- Caelestia Brightness Control ---#
        ", XF86MonBrightnessUp, global, caelestia:brightnessUp"
        ", XF86MonBrightnessDown, global, caelestia:brightnessDown"

        #--- Caelestia Media Control ---#
        ", XF86AudioPlay, global, caelestia:mediaToggle"
        ", XF86AudioPause, global, caelestia:mediaToggle"
        ", XF86AudioNext, global, caelestia:mediaNext"
        ", XF86AudioPrev, global, caelestia:mediaPrev"
        ", XF86AudioStop, global, caelestia:mediaStop"

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
        "SUPER_CTRL_SHIFT, R, exec, qs -c caelestia kill"
        "SUPER_CTRL_ALT, R, exec, qs -c caelestia kill; caelestia shell -d"
      ];

      # Mouse binds
      bindm = ["SUPER_ALT, mouse:272, resizewindow"];
    };
  };
}
