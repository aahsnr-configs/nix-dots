{ inputs, pkgs, ... }:
{
  imports = [
    inputs.dankMaterialShell.homeModules.dankMaterialShell.default
  ];
  programs.dankMaterialShell = {
    enable = true;
    enableSystemd = true;
    quickshell.package = inputs.quickshell.packages.${pkgs.system}.default;
    default.settings = {
      theme = "dark";
      dynamicTheming = true;
    };
  };
}
