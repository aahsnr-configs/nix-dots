{ pkgs, inputs, ... }:
{
  home = {
    packages = with pkgs; [
      app2unit
      brightnessctl
      fuzzel
      hyprpicker
      # no flake or nixpkg yet
      # hyprqt6engine
      inputs.hyprpicker.packages.${pkgs.system}.hyprpicker
      inputs.pyprland.packages.${pkgs.system}.pyprland
    ];
    file.".config/hypr/pyprland.toml".source = ./pyprland.toml;
  };
}
