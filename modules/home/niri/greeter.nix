{...}: {
  programs.dankMaterialShell.greeter = {
    enable = true;
    compositor.name = "niri";
  };
}
