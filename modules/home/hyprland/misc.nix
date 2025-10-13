{ pkgs, inputs, ... }:
{
  home = {
    packages = with pkgs; [
      brightnessctl
      fuzzel
      cliphist
      ydotool
      inputs.hyprpicker.packages.${pkgs.system}.hyprpicker
      inputs.pyprland.packages.${pkgs.system}.pyprland
    ];
    file.".config/hypr/pyprland.toml".source = ./pyprland.toml;
    file.".config/hypr/scheme/default.conf".source = ./scheme/default.conf;
  };
}
