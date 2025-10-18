{ ... }:
{
  wayland.windowManager.hyprland.settings = {
    # Start pyprland daemon
    layerrule = [
      "animation fade, caelestia-(drawers|background)"
      "animation fade, hyprpicker"
      "animation fade, logout_dialog"
      "animation fade, selection"
      "animation fade, wayfreeze"
      "animation popin 80%, launcher"
      "blur, caelestia-drawers"
      "blur,^(gtk-layer-shell)$"
      "blur, launcher"
      "ignorealpha 0.57, caelestia-drawers"
      "noanim, caelestia-(border-exclusion|area-picker)"
    ];

    windowrule = [
      "center 1, class:foot, title:nmtui"
      "center 1, class:nwg-look"
      "center 1, class:org\\.gnome\\.Settings"
      "center 1, class:org\\.pulseaudio\\.pavucontrol|yad-icon-browser"
      "float, class:blueman-manager"
      "float, class:com-atlauncher-App, title:ATLauncher Console"
      "float, class:com\\.github\\.GradienceTeam\\.Gradience"
      "float, class:feh"
      "float, class:file-roller"
      "float, class:foot, title:nmtui"
      "float, class:guifetch"
      "float, class:imv"
      "float, class:nwg-look"
      "float, class:org\\.gnome\\.FileRoller"
      "float, class:org\\.gnome\\.Settings"
      "float, class:org\\.pulseaudio\\.pavucontrol|yad-icon-browser"
      "float, class:org\\.quickshell"
      "float, class:system-config-printer"
      "float, class:wev"
      "float, class:yad"
      "float, class:zenity"
      "float, title:.* Properties"
      "float, title:(Select|Open)( a)? (File|Folder)(s)?"
      "float, title:Export Image as PNG"
      "float, title:File (Operation|Upload)( Progress)?"
      "float, title:Friends List, class:steam"
      "float, title:GIMP Crash Debug"
      "float, title:Library"
      "float, title:Picture(-| )in(-| )[Pp]icture"
      "float, title:Save As"
      "idleinhibit always, class:steam_app_[0-9]+"
      "immediate, class:steam_app_[0-9]+"
      "keepaspectratio, title:Picture(-| )in(-| )[Pp]icture"
      "move 100%-w-2% 100%-w-3%, title:Picture(-| )in(-| )[Pp]icture"
      "noblur, title:Fusion360|(Marking Menu), class:fusion360\\.exe"
      "nodim, xwayland:1, title:win[0-9]+"
      "noshadow, xwayland:1, title:win[0-9]+"
      "pin, title:Picture(-| )in(-| )[Pp]icture"
      "rounding 10, title:, class:steam"
      "rounding 10, xwayland:1, title:win[0-9]+"
      "size 50% 60%, class:nwg-look"
      "size 60% 70%, class:foot, title:nmtui"
      "size 60% 70%, class:org\\.pulseaudio\\.pavucontrol|yad-icon-browser"
      "size 70% 80%, class:org\\.gnome\\.Settings"
      "workspace special:communication, class:discord|equibop|vesktop|whatsapp"
      "workspace special:music, class:feishin|Spotify|Supersonic"
      "workspace special:music, initialTitle:Spotify( Free)?"
      "workspace special:sysmon, class:btop"
      "workspace special:todo, class:Todoist"

      # Pyprland scratchpad rules
      "float, class:^(foot-float)$"
      "workspace special silent, class:^(foot-float)$"
      "float, class:^(explorer)$"
      "workspace special silent, class:^(explorer)$"
      "float, class:^(lazygit)$"
      "workspace special silent, class:^(lazygit)$"
      "float, class:^(thunar)$"
      "workspace special silent, class:^(thunar)$"
    ];

    windowrulev2 = [
      # Animation Rules
      "animation slide down, class:^(explorer)$"
      "animation slide down, class:^(lazygit)$"
      "animation slide up, class:^(foot-float)$"

      # Dim Around Rules
      "dimaround, class:^(gcr-prompter)$"
      "dimaround, class:^(org.mate.polkit-mate-authentication-agent-1)$"
      "dimaround, class:^(xdg-desktop-portal-gtk)$"
      "dimaround, class:^(xdg-desktop-portal-hyprland)$"

      # Float Rules
      "float,class:^(com.github.Aylur.ags)$"
      "float,class:^(confirm)$"
      "float,class:^(confirmreset)$"
      "float,class:^(deluge)$"
      "float,class:^(dialog)$"
      "float,class:^(download)$"
      "float,class:^(error)$"
      "float,class:^(file_progress)$"
      "float,class:^(foot-float)$"
      "float,class:^(notification)$"
      "float,class:^(org.mate.polkit-mate-authentication-agent-1)$"
      "float,class:^(qt5ct)$"
      "float,class:^(qt6ct)$"
      "float,class:^(xdg-desktop-portal)$"
      "float,class:^(xdg-desktop-portal-gtk)$"
      "float,class:^(xdg-desktop-portal-hyprland)$"
      "float,title:^(branchdialog)$"
      "float,title:^(Confirm to replace files)"
      "float,title:^(File Operation Progress)"
      "float,title:^(Open File)$"


      # Idle Inhibit Rules
      "idleinhibit focus, class:^(brave)$, title:^(.*YouTube.*)$"
      "idleinhibit focus, class:^(mpv)$"
      "idleinhibit fullscreen, class:^(brave)$"

      # Minimum Size Rules
      "minsize 1 1, title:^()$,class:^(keepassxc)$"
      "minsize 1 1, title:^()$,class:^(steam)$"

      # No Blur/Animation/Focus Rules
      "noblur,class:^()$,title:^()$"
      "noanim,class:^(xwaylandvideobridge)$"
      "nofocus,class:^(xwaylandvideobridge)$"
      "noinitialfocus,class:^(xwaylandvideobridge)$"

      # Opacity Rules
      "opacity 0.0 override 0.0 override,class:^(xwaylandvideobridge)$"
      "opacity 0.80 0.80,class:^(ags)$"
      "opacity 0.90 0.70,class:^(org.mate.polkit-mate-authentication-agent-1)$"
      "opacity 0.90 0.70,class:^(pavucontrol)$"
      "opacity 0.90 0.80,class:^(Spotify)$"
      "opacity 0.90 0.80,class:^(steamwebhelper)$"
      "opacity 0.90 0.80,class:^(WebCord)$"
      "opacity 0.90 0.80,class:^(discord)$"
      "opacity 0.90 0.80,class:^(qt5ct)$"
      "opacity 0.90 0.90,class:^(explorer)$"
      "opacity 0.80 0.80,class:^(foot-float)$"
      "opacity 0.80 0.80,class:^(footclient)$"
      "opacity 0.80 0.80,class:^(foot)$"
      "opacity 0.80 0.80,class:^(kitty)$"
      "opacity 0.80 0.80,class:^(kitty-float)$"      
      "opacity 0.90 0.90,class:^(Thunar)$"
      "opacity 0.90 0.90,class:^(thunar)$" # Added for scratchpad
      # "opacity 0.90 0.90,class:^(emacs)$"
      "opacity 0.95 0.95,class:^(Code)$"

      # Size Rules
      "size 40% 20%,class:^(org.mate.polkit-mate-authentication-agent-1)$"
      "size 40% 30%,title:^(Volume Control)$"
      "size 40% 40%,class:^(Download)$"
      "size 40% 40%,title:^(Open File)$"
      "size 40% 40%,title:^(Save File)$"
      "size 70% 70%,class:^(foot-float)$"

      # Stay Focused Rules
      "stayfocused, title:^()$,class:^(keepassxc)$"
      "stayfocused, title:^()$,class:^(steam)$"
    ];
  };
}
