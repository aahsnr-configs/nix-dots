{ config, pkgs, ... }:
{
  programs.eza = {
    enable = true;
    enableZshIntegration = true;
    icons = "always";
    colors = "always";
    git = true;
    extraOptions = [
      "--group-directories-first"
      "--header"
    ];
  };
}
