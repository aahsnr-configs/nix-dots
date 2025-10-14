{ ... }:
{
  wayland.windowManager.hyprland.settings = {
    env = [
      "QT_AUTO_SCREEN_SCALE_FACTOR,1"
      "QT_QPA_PLATFORM,wayland"
      "QT_QPA_PLATFORM,wayland;xcb"
      "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
      "QT_QPA_PLATFORMTHEME,qt6ct"

      "AQ_DRM_DEVICES=,/dev/dri/card1:/dev/dri/card0"

      "SDL_VIDEODRIVER,wayland"
      "GDK_BACKEND,wayland,x11,*"
      "CLUTTER_BACKEND,wayland"

      "GBM_BACKEND,nvidia-drm"
      "__GLX_VENDOR_LIBRARY_NAME,nvidia"
      "LIBVA_DRIVER_NAME,nvidia"

      "XDG_CURRENT_DESKTOP,Hyprland"
      "XDG_SESSION_DESKTOP,Hyprland"
      "XDG_SESSION_TYPE,wayland"
    ];
  };
}
