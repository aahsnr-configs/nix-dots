{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      nwg-dock-hyprland
    ];
    #file.".config/hypr/shaders/blue-light-filter.glsl".source  = ./blue-light-filter.glsl;
    #file.".config/hypr/pyprland.toml".source = ./pyprland.toml;
  };
}
