{inputs, ...}: {
  imports = [
    inputs.dankMaterialShell.homeModules.dankMaterialShell.default
  ];

  programs.dankMaterialShell.greeter = {
    enable = true;
    compositor.name = "niri";
  };
}
