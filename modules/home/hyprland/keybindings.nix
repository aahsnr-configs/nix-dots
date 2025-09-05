{ ... }: {
  wayland.windowManager.hyprland = {
    settings = {
      # Binds are actions that are executed on key press
      bind = [
        #--- Applications ---#
        "Super, Return, exec, app2unit -- kitty"
        "Super, E, exec, emacsclient -c -a 'emacs'"
        "Super, B, exec, app2unit -- zen"
        "Super, Z, exec, app2unit -- zotero"
        "Super, T, exec, app2unit -- thunar"

        #--- Scratchpads ---#
        "Super+Shift, RETURN, exec, pypr toggle term"
        "Super+Shift, Y, exec, pypr toggle yazi"

        #--- Window Actions ---#
        "Super, Q, killactive,"
        "Super, F, fullscreen, 0"
        "Super, Space, togglefloating,"
        "Super, S, togglesplit,"

        # Change Focus
        "Super, left,  movefocus, l"
        "Super, right, movefocus, r"
        "Super, up,    movefocus, u"
        "Super, down,  movefocus, d"

        # Move Focused Window
        "Super+Shift, left,  movewindow, l"
        "Super+Shift, right, movewindow, r"
        "Super+Shift, up,    movewindow, u"
        "Super+Shift, down,  movewindow, d"

        # Resize Focused Window
        "Super+Ctrl, left,  resizeactive, -45 0"
        "Super+Ctrl, right, resizeactive, 45 0"
        "Super+Ctrl, up,    resizeactive, 0 -45"
        "Super+Ctrl, down,  resizeactive, 0 45"

        # Switch between windows
        "Super, Tab, cyclenext, bringactivetotop,"

        #--- App Launcher ---#
        "Super, D, exec, caelestia:launcher"

        #--- Caelestia Actions & Misc ---#
        "Super, F1, exec, gamemode"
        "Ctrl+Alt, Delete, exec, caelestia:session"
        "Super, K, exec, caelestia:showall"
        "Super, L, exec, caelestia:lock"

        #--- Workspaces ---#
        "Super, 1, exec, wsaction workspace 1"
        "Super, 2, exec, wsaction workspace 2"
        "Super, 3, exec, wsaction workspace 3"
        "Super, 4, exec, wsaction workspace 4"
        "Super, 5, exec, wsaction workspace 5"
        "Super, 6, exec, wsaction workspace 6"
        "Super, 7, exec, wsaction workspace 7"
        "Super, 8, exec, wsaction workspace 8"
        "Super, 9, exec, wsaction workspace 9"
        "Super, 0, exec, wsaction workspace 10"

        # Move window to workspace
        "Ctrl+Super+Alt, 1, exec, wsaction movetoworkspace 1"
        "Ctrl+Super+Alt, 2, exec, wsaction movetoworkspace 2"
        "Ctrl+Super+Alt, 3, exec, wsaction movetoworkspace 3"
        "Ctrl+Super+Alt, 4, exec, wsaction movetoworkspace 4"
        "Ctrl+Super+Alt, 5, exec, wsaction movetoworkspace 5"
        "Ctrl+Super+Alt, 6, exec, wsaction movetoworkspace 6"
        "Ctrl+Super+Alt, 7, exec, wsaction movetoworkspace 7"
        "Ctrl+Super+Alt, 8, exec, wsaction movetoworkspace 8"
        "Ctrl+Super+Alt, 9, exec, wsaction movetoworkspace 9"
        "Ctrl+Super+Alt, 0, exec, wsaction movetoworkspace 10"

        # Special Workspace Actions
        "Super, C, movetoworkspace, special"
        "Super+Alt, S, exec, caelestia toggle specialws"
        "Ctrl+Shift, Escape, exec, caelestia toggle sysmon"
        "Super, M, exec, caelestia toggle music"
        "Super+Alt, C, exec, caelestia toggle communication"
        "Super, R, exec, caelestia toggle todo"

        #--- Utilities ---#
        "Super+Shift, S, exec, caelestia:screenshotFreeze"
        "Super+Shift+Alt, S, exec, caelestia:screenshot"
        "Super+Alt, R, exec, caelestia record -s"
        "Ctrl+Alt, R, exec, caelestia record"
        "Super+Shift+Alt, R, exec, caelestia record -r"
        "Super+Shift, C, exec, hyprpicker -a"

        #--- System Actions ---#
        "Super+Shift, L, exec, systemctl suspend-then-hibernate"

        #--- Clipboard and Emoji Picker ---#
        "Super, V, exec, pkill fuzzel || caelestia clipboard"
        "Super+Alt, V, exec, pkill fuzzel || caelestia clipboard -d"
        "Super, Period, exec, pkill fuzzel || caelestia emoji -p"
      ];

      # Binds that are repeatable
      binde = [
        "Ctrl+Alt, right, workspace, +1"
        "Ctrl+Alt, left, workspace, -1"
        "Ctrl+Super+Shift, right, movetoworkspace, +1"
        "Ctrl+Super+Shift, left, movetoworkspace, -1"
        ", XF86AudioRaiseVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
        ", XF86AudioLowerVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
      ];

      # Binds that are executed on lockscreen
      bindl = [
        "Ctrl+Alt, C, exec, caelestia:clearNotifs"
        "Super+Alt, L, exec, caelestia shell -d; caelestia:lock"
        ", XF86MonBrightnessUp, exec, caelestia:brightnessUp"
        ", XF86MonBrightnessDown, exec, caelestia:brightnessDown"
        ", XF86AudioPlay, exec, caelestia:mediaToggle"
        ", XF86AudioPause, exec, caelestia:mediaToggle"
        ", XF86AudioNext, exec, caelestia:mediaNext"
        ", XF86AudioPrev, exec, caelestia:mediaPrev"
        ", XF86AudioStop, exec, caelestia:mediaStop"
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ", Print, exec, caelestia screenshot"
        ", switch:on:lid, exec, disable-lid"
      ];

      # Binds that are executed on key release
      bindr = [
        "Super+Ctrl+Shift, R, exec, qs -c caelestia kill"
        "Super+Ctrl+Alt, R, exec, qs -c caelestia kill; caelestia shell -d"
      ];

      # Mouse binds
      bindm = [ "Super+Alt, mouse:272, resizewindow" ];
    };
  };
}
