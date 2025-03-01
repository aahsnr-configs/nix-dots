{ config, pkgs, ... }:

{
  wayland.windowManager.hyprland.settings = {
    env = [
      # "XDG_SESSION_DESKTOP,Hyprland"
      # "XDG_SESSION_TYPE,wayland"
      # "XDG_CURRENT_DESKTOP,Hyprland"
      # "XDG_CURRENT_SESSION,Hyprland"
      # "TDESKTOP_DISABLE_GTK_INTEGRATION,1"
      # "CLUTTER_BACKEND,wayland"
      # "BEMENU_BACKEND,wayland"
      #"GDK_SCALE,1.8"
      # "GDK_DPI_SCALE,1.7"
      # "ELM_SCALE,1.5"
      #"_JAVA_OPTIONS, -Dawt.useSystemAAFontSettings=on"
      # "MOZ_ENABLE_WAYLAND,1"
      # "QT_QPA_PLATFORMTHEME,qt5ct"
      # "QT_SCALE_FACTOR,1.4"
      # "QT_QPA_PLATFORM,wayland;xcb"
      # "QT_WAYLAND_FORCE_DPI,physical"
      # "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
      # "ELM_DISPLAY,wl"
      # "ECORE_EVAS_ENGINE,wayland_egl"
      # "ELM_ENGINE,wayland_egl"
      # "ELM_ACCEL,opengl"
      # "SDL_VIDEODRIVER,wayland"
      # "SAL_USE_VCLPLUGIN,gtk4"
      # "CLUTTER_BACKEND,wayland"
      #"WLR_DRM_DEVICES,/dev/dri/card1"
      # "GDK_BACKEND,wayland,x11"
      # "NO_AT_BRIDGE,1"
      # "WINIT_UNIX_BACKEND,wayland"
      # "__GLX_VENDOR_LIBRARY_NAME,nvidia"
      # "GBM_BACKEND,nvidia"
      # "LIBVA_DRIVER_NAME,nvidia"
      # "WLR_NO_HARDWARE_CURSORS,1"
      # "WLR_RENDERER_ALLOW_SOFTWARE,1"
      # "LIBSEAT_BACKEND,logind"
    ];
  };
}
