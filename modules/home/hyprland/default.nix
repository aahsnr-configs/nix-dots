{ pkgs, inputs, ... }:
{

  wayland.windowManager.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    xwayland.enable = true;
    systemd.enable = false; # messes with uwsm
    plugins = with inputs; [
      hyprland-plugins.packages.${pkgs.system}.hyprexpo
      hyprland-plugins.packages.${pkgs.system}.hyprscrolling
    ];
  };

  home = {
    packages = with inputs; [
      hyprpicker.packages.${pkgs.system}.hyprpicker
      pyprland.packages.${pkgs.system}.pyprland
    ];
    file = {
      ".config/hypr/hyprland.conf".source = ./hyprland.conf;
      ".config/hypr/conf/autostart.conf".source = ./conf/autostart.conf;
      ".config/hypr/conf/env.conf".source = ./conf/env.conf;
      ".config/hypr/conf/keybindings.conf".source = ./conf/keybindings.conf;
      ".config/hypr/conf/monitor.conf".source = ./conf/monitor.conf;
      ".config/hypr/conf/rules.conf".source = ./conf/rules.conf;
      ".config/hypr/conf/settings.conf".source = ./conf/settings.conf;
      ".config/hypr/pyprland.toml".source = ./pyprland.toml;
    };
  };
}
