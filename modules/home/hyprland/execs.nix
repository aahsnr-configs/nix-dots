{ ... }:
{
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "pypr"
      "mpris-proxy"
      "caelestia resizer -d"
      "caelestia shell -d"
    ];
  };
}
