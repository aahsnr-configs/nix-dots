{ pkgs, inputs, ... }:

{
  home = {
    packages = with pkgs; [
      inputs.pyprland.packages.${pkgs.system}.pyprland
      inputs.hyprland-contrib.packages.${pkgs.system}.grimblast
      hyprnome
      hyprsunset
      hyprlandPlugins.hyprscroller
    ];
    #file.".config/hypr/shaders/blue-light-filter.glsl".source  = ./blue-light-filter.glsl;
    file.".config/hypr/pyprland.toml".source = ./pyprland.toml;
  };

  wayland.windowManager.hyprland.portalPackage = inputs.xdg-portal-hyprland.packages.${pkgs.system}.default;

}
