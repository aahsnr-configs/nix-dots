{...}: {
  environment = {
    sessionVariables = {
      _JAVA_AWT_WM_NONEREPARENTING = "1";
      ANKI_WAYLAND = "1";
      BEMENU_BACKEND = "wayland";
      CLUTTER_BACKEND = "wayland";
      DIRENV_LOG_FORMAT = "";
      DISABLE_QT5_COMPAT = "0";
      ECORE_EVAS_ENGINE = "wayland_egl";
      ELM_ACCEL = "opengl";
      ELM_DISPLAY = "wl";
      ELM_ENGINE = "wayland_egl";
      ELM_SCALE = "1.5";
      FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
      GDK_BACKEND = "wayland,x11";
      GDK_DPI_SCALE = "1.7";
      #GDK_SCALE = "1.8";
      LIBSEAT_BACKEND = "logind";
      MOZ_ENABLE_WAYLAND = "1";
      NIXOS_OZONE_WL = "1";
      NO_AT_BRIDGE = "1";
      QT_FONT_DPI = "120 clementine";
      QT_QPA_PLATFORM = "wayland;xcb";
      QT_QPA_PLATFORMTHEME = "kvantum";
      QT_SCALE_FACTOR = "1.7";
      QT_WAYLAND_DISABLE_WINDOWDEROCATION = "1";
      QT_WAYLAND_FORCE_DPI = "physical";
      SAL_USE_VCLPLUGIN = "gtk4";
      SDL_VIDEODRIVER = "wayland";
      TDESKTOP_DISABLE_GTK_INTEGRATION = "1";
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_CURRENT_SESSION = "Hyprland";
      XDG_SESSION_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      WINIT_UNIX_BACKEND = "wayland";
      WLR_DRM_NO_ATOMIC = "1";
      WLR_BACKEND = "vulkan";
      WLR_NO_HARDWARE_CURSORS = "1";
      WLR_RENDERER = "vulkan";
      WLR_RENDERER_ALLOW_SOFTWARE = "1";
    };
  };
}
