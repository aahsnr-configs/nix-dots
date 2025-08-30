{ ... }: {
  wayland.windowManager.hyprland = {
    settings = {
      bind = [
        #--- Terminal --
        "SUPER, Return, exec, kitty"

        #--- Scratchpads
        "SUPERSHIFT, RETURN, exec, pypr toggle term"
        "SUPERSHIFT, Y, exec, pypr toggle yazi"

        #--- Window Management --
        "SUPER, Q, killactive,"
        "SUPER, F, fullscreen, 0"
        "SUPER, Space, togglefloating,"
        "SUPER, S, togglesplit,"

        # Change Focus
        "SUPER, left,  movefocus, l"
        "SUPER, right, movefocus, r"
        "SUPER, up,    movefocus, u"
        "SUPER, down,  movefocus, d"

        # Move Focused Window
        "SUPERSHIFT, left,  movewindow, l"
        "SUPERSHIFT, right, movewindow, r"
        "SUPERSHIFT, up,    movewindow, u"
        "SUPERSHIFT, down,  movewindow, d"

        # Resize Focused Window
        "SUPERCTRL, left,  resizeactive, -45 0"
        "SUPERCTRL, right, resizeactive, 45 0"
        "SUPERCTRL, up,    resizeactive, 0 -45"
        "SUPERCTRL, down,  resizeactive, 0 45"

        # Switch between windows
        "SUPERSHIFT, Tab, cyclenext,"
        "SUPERSHIFT, Tab, bringactivetotop,"

        #-- GUI Apps --
        "SUPER, D, exec, pkill anyrun || anyrun"
        "SUPER, E, exec, emacsclient -c -a 'emacs'"
        "SUPER, B, exec, zen"
        "SUPER, Z, exec, zotero"
        "SUPER, T, exec, pypr toggle filemanager"
        #"SUPERSHIFT, R, exec, jabref"

        #-- Workspaces --
        "CTRL_ALT, left, exec, hyprnome --previous"
        "CTRL_ALT, right, exec, hyprnome"
        "SUPER_CTRL_ALT, left, exec, hyprnome --previous --move"
        "SUPER_CTRL_ALT, right, exec, hyprnome --move"

        #-- Hyprpanel --
        "SUPER, X, exec, hyprpanel t powermenu"
        "SUPER, M, exec, actions --menu"
        "SUPER, C, exec, actions --calendar"
        "SUPER, F1, exec, gamemode"
        "SUPER, C, movetoworkspace, special"
      ];

      bindle = [
        #-- Screenshot --
        ",XF86Launch4,   exec, ags -r 'recorder.start()'"
        ",Print,         exec, ags -r 'recorder.screenshot()'"
        "SHIFT,Print,    exec, ags -r 'recorder.screenshot(true)'"

        #-- Volume --
        ", XF86AudioLowerVolume, exec, pactl set-sink-volume @DEFAULT_SINK@ -5%"
        ", XF86AudioRaiseVolume, exec, pactl set-sink-volume @DEFAULT_SINK@ +5%"
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_SINK@ toggle"

        #-- Brightness --
        ",XF86MonBrightnessUp, exec, lightctl up 5"
        ",XF86MonBrightnessDown, exec, lightctl down 5"
      ];

      bindm = [
        "SUPER, mouse:272, movewindow"
        "SUPER, mouse:273, resizewindow"
        "SUPER ALT, mouse:272, resizewindow"
      ];
    };
  };
}
