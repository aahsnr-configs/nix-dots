{ config, pkgs, ... }:

{
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "GDK_DPI_SCALE=0.8 uwsm app -- hyprpanel"
      "uwsm app -- hyprsunset -t 5500"
      "uwsm app -- pypr"
      "uwsm app -- turnoffMonitor"
    ];
  };

}
