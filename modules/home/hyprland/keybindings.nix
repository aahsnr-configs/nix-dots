{ pkgs, ... }:
{
  wayland.windowManager.hyprland = {
    settings = {
      exec = [ "hyprctl dispatch submap global" ];

      submap = [ "global" ];

      bind = [
        #--- Applications --
        ## Terminal
        "Super, Return, exec, app2unit -- kitty"

        ## GUI Apps
        # App2unit not needed for emacsclient since
        # process is already managed by systemd
        "Super, E, exec, emacsclient -c -a 'emacs'"
        "Super, B, exec, app2unit -- zen"
        "Super, Z, exec, app2unit -- zotero"
        "Super, T, exec, app2unit -- thunar"

        #--- Scratchpads --
        "Super+Shift, RETURN, exec, pypr toggle term"
        "Super+Shift, Y, exec, pypr toggle yazi"

        #--- Window Actions --
        ## Primary Actions
        "Super, Q, killactive,"
        "Super, F, fullscreen, 0"
        "Super, Space, togglefloating,"
        "Super, S, togglesplit,"

        ## Change Focus
        "Super, left,  movefocus, l"
        "Super, right, movefocus, r"
        "Super, up,    movefocus, u"
        "Super, down,  movefocus, d"

        ## Move Focused Window
        "Super+Shift, left,  movewindow, l"
        "Super+Shift, right, movewindow, r"
        "Super+Shift, up,    movewindow, u"
        "Super+Shift, down,  movewindow, d"

        ## Resize Focused Window
        "Super+Ctrl, left,  resizeactive, -45 0"
        "Super+Ctrl, right, resizeactive, 45 0"
        "Super+Ctrl, up,    resizeactive, 0 -45"
        "Super+Ctrl, down,  resizeactive, 0 45"

        ## Switch between windows
        "Super+Shift, Tab, cyclenext,"
        "Super+Shift, Tab, bringactivetotop,"

        #--- App Launcher --
        "Super, D, exec, caelestia:launcher"

        #--- Misc --
        "Super, F1, exec, gamemode"
        "Super, C, movetoworkspace, special"
        "Ctrl+Alt, Delete, exec, caelestia:session"
        "Super, K, exec, caelestia:showall"
        "Super, L, exec, caelestia:lock"

        #--- Workspaces --
        ## Go to workspace no.
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

        ## Move window to workspace no.
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

        ## Special Workspaces Toggles
        "Super, S, exec, caelestia toggle specialws"
        "Ctrl+Shift, Escape, exec, caelestia toggle sysmon" # toggle system monitor
        "Super, M, exec, caelestia toggle music" # toggle music
        "Super, C, exec, caelestia toggle communication" # toggle communication
        "Super, R, exec, caelestia toggle todo" # toggle todo

        #--- Utilities --
        "Super+Shift, S, global, caelestia:screenshotFreeze" # Capture region (freeze)
        "Super+Shift+Alt, S, global, caelestia:screenshot" # Capture region
        "Super+Alt, R, exec, caelestia record -s" # Record screen with sound
        "Ctrl+Alt, R, exec, caelestia record" # Record screen
        "Super+Shift+Alt, R, exec, caelestia record -r" # Record region
        "Super+Shift, C, exec, hyprpicker -a" # Colour picker

        #--- Sleep --
        "Super+Shift, L, exec, systemctl suspend-then-hibernate"

        #--- Clipboard and Emoji Picker --
        "Super, V, exec, pkill fuzzel || caelestia clipboard"
        "Super+Alt, V, exec, pkill fuzzel || caelestia clipboard -d"
        "Super, Period, exec, pkill fuzzel || caelestia emoji -p"

      ];

      binde = [
        ## Go to workspace -1/+1
        "Ctrl+Alt, right, workspace, +1"
        "Ctrl+Alt, left, workspace, -1"

        ## Move window to workspace -1/+1
        "Ctrl+Super+Shift, right, movetoworkspace, +1"
        "Ctrl+Super+Shift, left, movetoworkspace, -1"

        ## Volume Control
        ", XF86AudioRaiseVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ $volumeStep%+"
        ", XF86AudioLowerVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume @DEFAULT_AUDIO_SINK@ $volumeStep%-"

      ];

      bindi = [
        "Super, Super_L, global, caelestia:launcher"
      ];

      bindin = [
        "Super, catchall, global, caelestia:launcherInterrupt"
        "Super, mouse:272, global, caelestia:launcherInterrupt"
        "Super, mouse:273, global, caelestia:launcherInterrupt"
        "Super, mouse:274, global, caelestia:launcherInterrupt"
        "Super, mouse:275, global, caelestia:launcherInterrupt"
        "Super, mouse:276, global, caelestia:launcherInterrupt"
        "Super, mouse:277, global, caelestia:launcherInterrupt"
        "Super, mouse_up, global, caelestia:launcherInterrupt"
        "Super, mouse_down, global, caelestia:launcherInterrupt"
      ];

      bindl = [
        #--- Misc --
        "Ctrl+Alt, C, global, caelestia:clearNotifs"

        #--- Restore lock --
        "Super+Alt, L, exec, caelestia shell -d"
        "Super+Alt, L, global, caelestia:lock"

        #--- Brightness --
        ", XF86MonBrightnessUp, global, caelestia:brightnessUp"
        ", XF86MonBrightnessDown, global, caelestia:brightnessDown"

        #--- Media --
        "Super+Ctrl, Space, global, caelestia:mediaToggle"
        ", XF86AudioPlay, global, caelestia:mediaToggle"
        ", XF86AudioPause, global, caelestia:mediaToggle"
        "Super+Ctrl, Equal, global, caelestia:mediaNext"
        ", XF86AudioNext, global, caelestia:mediaNext"
        "Super+Ctrl, Minus, global, caelestia:mediaPrev"
        ", XF86AudioPrev, global, caelestia:mediaPrev"
        ", XF86AudioStop, global, caelestia:mediaStop"

        #--- Volume Control --
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

        #--- Utilities --
        ", Print, exec, caelestia screenshot" # Full screen capture > clipboard
      ];

      bindr = [
        #-- Kill/restart --
        "Super+Ctrl+Shift, R, exec, qs -c caelestia kill"
        "Super_Alt, R, exec, qs -c caelestia kill; caelestia shell -d"
      ];

      bindm = [
        "Super+Alt, mouse:272, resizewindow"
      ];
    };
  };
}
