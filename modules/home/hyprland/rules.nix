{ inputs, pkgs, ... }:

{
  wayland.windowManager.hyprland.settings = {
    layerrule = [
      "blur,^(com.github.Aylur.ags)$"
      "blur,^(eww_powermenu)$"
      "blur,^(gtk-layer-shell)$"
    ];

    windowrulev2 = [
      #--- Animation Rules
      "animation slide up, class:^(foot-float)$"
      "animation slide up, class:^(kitty-float)$"
      "animation slide down, class:^(explorer)$"

      #--- Floating Rules
      "float,class:^(deluge)$"
      "float,class:^(foot-float)$"
      "float,class:^(kitty-float)$"
      "float,class:^(explorer)$"
      "float,class:^(foot-float)$"
      "float,class:^(org.mate.polkit-mate-authentication-agent-1)$"
      "float,class:^(qt5ct)$"
      "float,class:^(qt6ct)$"
      "float,class:^(file_progress)$"
      "float,class:^(confirm)$"
      "float,class:^(dialog)$"
      "float,class:^(download)$"
      "float,class:^(notification)$"
      "float,class:^(error)$"
      "float,class:^(confirmreset)$"
      "float,title:^(Open File)$"
      "float,title:^(branchdialog)$"
      "float,title:^(Confirm to replace files)"
      "float,title:^(File Operation Progress)"
      "float,class:^(com.github.Aylur.ags)$"
      "float,class:^(xdg-desktop-portal)$"
      "float,class:^(xdg-desktop-portal-gtk)$"
      "float,class:^(xdg-desktop-portal-hyprland)$"

      #--- IdleInhibit Rules
      "idleinhibit focus, class:^(mpv)$"
      "idleinhibit focus, class:^(brave)$, title:^(.*YouTube.*)$"
      "idleinhibit fullscreen, class:^(brave)$"

      #--- DimAround Rules
      "dimaround, class:^(gcr-prompter)$"
      "dimaround, class:^(xdg-desktop-portal-gtk)$"
      "dimaround, class:^(xdg-desktop-portal-hyprland)$"
      "dimaround, class:^(org.mate.polkit-mate-authentication-agent-1)$"

      #--- Opacity Rules
      "opacity 0.90 0.90,class:^(foot-float)$"
      "opacity 0.90 0.90,class:^(explorer)$"
      #"windowrulev2 = opacity 0.90 0.90,class:^(emacs)$"
      "opacity 0.90 0.90,class:^(kitty)$"
      "opacity 0.95 0.95,class:^(foot)$"
      "opacity 0.90 0.90,class:^(footclient)$"
      "opacity 0.90 0.80,class:^(steamwebhelper)$"
      "opacity 0.90 0.80,class:^(Spotify)$"
      "opacity 0.95 0.95,class:^(Code)$"
      "opacity 0.90 0.90,class:^(Thunar)$"
      "opacity 0.90 0.80,class:^(file-roller)$"
      "opacity 0.80 0.80,class:^(ags)$"
      "opacity 0.90 0.80,class:^(qt5ct)$"
      "opacity 0.90 0.80,class:^(discord)$"
      "opacity 0.90 0.80,class:^(WebCord)$"
      "opacity 0.90 0.70,class:^(pavucontrol)$"
      "opacity 0.90 0.70,class:^(org.mate.polkit-mate-authentication-agent-1)$"

      #--- Size Rules
      "size 40% 40%,class:^(deluge-gtk)$"
      "size 40% 20%,class:^(org.mate.polkit-mate-authentication-agent-1)$"
      "size 70% 70%,class:^(foot-float)$"
      #"size 90% 90%,class:^(explorer)$"
      "size 40% 40%,class:^(Download)$"
      "size 40% 40%,title:^(Open File)$"
      "size 40% 40%,title:^(Save File)$"
      "size 40% 30%,title:^(Volume Control)$"

      #--- Workspace Rules
      "workspace special silent,class:^(foot-float)$"
      "workspace special silent,class:^(explorer)$"

      #--- XwaylandVideoBridge Specific Rules
      "opacity 0.0 override 0.0 override,class:^(xwaylandvideobridge)$"
      "noanim,class:^(xwaylandvideobridge)$"
      "nofocus,class:^(xwaylandvideobridge)$"
      "noinitialfocus,class:^(xwaylandvideobridge)$"
      "stayfocused, title:^()$,class:^(steam)$"
      "minsize 1 1, title:^()$,class:^(steam)$"
      "stayfocused, title:^()$,class:^(keepassxc)$"
      "minsize 1 1, title:^()$,class:^(keepassxc)$"

      #--- Misc. Rules
      "noblur,class:^()$,title:^()$"

    ];
  };
}
