{ inputs, pkgs, ... }:
{
  services.hyprpolkitagent = {
    enable = true;
    package = inputs.hyprpolkitagent.packages.${pkgs.system}.hyprpolkitagent;
  };
}
