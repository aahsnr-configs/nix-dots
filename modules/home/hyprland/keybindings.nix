{ ... }:
{
  wayland.windowManager.hyprland = {
    settings = {
      exec-once = [ "hyprctl dispatch submap global" ];
      submap = [ "global" ];
      bind = [
        "Super, D, global, caelestia:launcher"
        "Super,X, global, caelestia:session"
        "Super, K, global, caelestia:showall"

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

        "Ctrl+Super, 1, exec, wsaction -g workspace 1"
        "Ctrl+Super, 2, exec, wsaction -g workspace 2"
        "Ctrl+Super, 3, exec, wsaction -g workspace 3"
        "Ctrl+Super, 4, exec, wsaction -g workspace 4"
        "Ctrl+Super, 5, exec, wsaction -g workspace 5"
        "Ctrl+Super, 6, exec, wsaction -g workspace 6"
        "Ctrl+Super, 7, exec, wsaction -g workspace 7"
        "Ctrl+Super, 8, exec, wsaction -g workspace 8"
        "Ctrl+Super, 9, exec, wsaction -g workspace 9"
        "Ctrl+Super, 0, exec, wsaction -g workspace 10"

        "Ctrl+Super, mouse_down, workspace, -10"
        "Ctrl+Super, mouse_up, workspace, +10"

        "Super+Alt, 1, exec, wsaction movetoworkspace 1"
        "Super+Alt, 2, exec, wsaction movetoworkspace 2"
        "Super+Alt, 3, exec, wsaction movetoworkspace 3"
        "Super+Alt, 4, exec, wsaction movetoworkspace 4"
        "Super+Alt, 5, exec, wsaction movetoworkspace 5"
        "Super+Alt, 6, exec, wsaction movetoworkspace 6"
        "Super+Alt, 7, exec, wsaction movetoworkspace 7"
        "Super+Alt, 8, exec, wsaction movetoworkspace 8"
        "Super+Alt, 9, exec, wsaction movetoworkspace 9"
        "Super+Alt, 0, exec, wsaction movetoworkspace 10"

        "Ctrl+Super+Alt, 1, exec, wsaction -g movetoworkspace 1"
        "Ctrl+Super+Alt, 2, exec, wsaction -g movetoworkspace 2"
        "Ctrl+Super+Alt, 3, exec, wsaction -g movetoworkspace 3"
        "Ctrl+Super+Alt, 4, exec, wsaction -g movetoworkspace 4"
        "Ctrl+Super+Alt, 5, exec, wsaction -g movetoworkspace 5"
        "Ctrl+Super+Alt, 6, exec, wsaction -g movetoworkspace 6"
        "Ctrl+Super+Alt, 7, exec, wsaction -g movetoworkspace 7"
        "Ctrl+Super+Alt, 8, exec, wsaction -g movetoworkspace 8"
        "Ctrl+Super+Alt, 9, exec, wsaction -g movetoworkspace 9"
        "Ctrl+Super+Alt, 0, exec, wsaction -g movetoworkspace 10"

        "Super, Comma, togglegroup"
        "Super, U, moveoutofgroup"
        "Super+Shift, Comma, lockactivegroup, toggle"

        "Super, left,  movefocus, l"
        "Super, right, movefocus, r"
        "Super, up,    movefocus, u"
        "Super, down,  movefocus, d"

        "Super+Shift, left,  movewindow, l"
        "Super+Shift, right, movewindow, r"
        "Super+Shift, up,    movewindow, u"
        "Super+Shift, down,  movewindow, d"

        "Super+Ctrl, left,  resizeactive, -45 0"
        "Super+Ctrl, right, resizeactive, 45 0"
        "Super+Ctrl, up,    resizeactive, 0 -45"
        "Super+Ctrl, down,  resizeactive, 0 45"

        "Super, Q, killactive,"
        "Super, F, fullscreen, 0"
        "Super, Space, togglefloating,"

        "Super, S, exec, caelestia toggle specialws"
        "Ctrl+Shift, Escape, exec, caelestia toggle sysmon"
        "Super, M, exec, caelestia toggle music"
        "Super, C, exec, caelestia toggle communication"
        "Super, R, exec, caelestia toggle todo"

        "Super, Return, exec, footclient"

        "Super+Shift, RETURN, exec, pypr toggle term"
        "Super+Shift, Y, exec, pypr toggle tuifm"
        "Super+Shift, G, exec, pypr toggle tuigit"

        "Super, E, exec, emacsclient -c -a 'emacs'"
        "Super, B, exec, zen-browser"
        "Super, Z, exec, zotero"
        "Super, T, exec, thunar"

        "Super+Shift, S, global, caelestia:screenshotFreeze"
        "Super+Alt, R, exec, caelestia record -s"
        "Ctrl+Alt, R, exec, caelestia record"
        "Super+Shift+Alt, R, exec, caelestia record -r"
        "Super+Shift, C, exec, hyprpicker -a"

        "Super+Shift, L, exec, systemctl suspend-then-hibernate"

        "Super, V, exec, pkill fuzzel || caelestia clipboard"
        "Super+Alt, V, exec, pkill fuzzel || caelestia clipboard -d"
        "Super, Period, exec, pkill fuzzel || caelestia emoji -p"
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

      binde = [
        "Ctrl+Alt, right, workspace, +1"
        "Ctrl+Alt, left, workspace, -1"

        "Ctrl+Super+Alt, right, movetoworkspace, +1"
        "Ctrl+Super+Alt, left, movetoworkspace, -1"

        "Alt, Tab, cyclenext, activewindow"
        "Alt, Tab, cyclenext, prev, activewindow"
        "Ctrl+Alt, Tab, changegroupactive, f"
        "Ctrl+Shift+Alt, Tab, changegroupactive, b"

        "Super+Alt, left, moveactive, -10 0"
        "Super+Alt, right, moveactive, 10 0"
        "Super+Alt, up, moveactive, 0 -10"
        "Super+Alt, down, moveactive, 0 10"
      ];

      bindl = [
        "Ctrl+Alt, C, global, caelestia:clearNotifs"
        "Super+Alt, L, exec, caelestia shell -d"
        "Super+Alt, L, global, caelestia:lock"
        ", XF86MonBrightnessUp, global, caelestia:brightnessUp"
        ", XF86MonBrightnessDown, global, caelestia:brightnessDown"
        ", XF86AudioPlay, global, caelestia:mediaToggle"
        ", XF86AudioPause, global, caelestia:mediaToggle"
        ", XF86AudioNext, global, caelestia:mediaNext"
        ", XF86AudioPrev, global, caelestia:mediaPrev"
        ", XF86AudioStop, global, caelestia:mediaStop"

        ", Print, exec, caelestia screenshot"
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

        ''Ctrl+Shift+Alt, V, exec, sleep 0.5s && ydotool type -d 1 "$(cliphist list | head -1 | cliphist decode)"''
      ];

      bindle = [
        ", XF86AudioRaiseVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ $volumeStep%+"
        ", XF86AudioLowerVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume @DEFAULT_AUDIO_SINK@ $volumeStep%-"
      ];

      bindr = [
        "Ctrl+Super+Shift, R, exec, qs -c caelestia kill"
        "Ctrl+Super+Alt, R, exec, qs -c caelestia kill; caelestia shell -d"
      ];

      bindm = [
        "Super, mouse:272, movewindow"
        "Super, mouse:273, resizewindow"
      ];
    };
  };
}
