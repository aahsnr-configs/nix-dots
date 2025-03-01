{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    libsixel
  ];

  programs.foot = {
    package = pkgs.foot;
    enable = true;
    server.enable = false;
    settings = {
      main = {
        pad="8x8";
        initial-window-size-chars="82x23";
        resize-delay-ms="50";
        font = "JetBrainsMono Nerd Font:size=24";
        dpi-aware = "yes";
      };
      scrollback = {
        lines = "1000";
        multiplier = "3.0";
      };
      cursor = {
        style = "block";
        blink = "yes";
      };
      mouse = {
        hide-when-typing = "yes";
        alternate-scroll-mode = "yes";
      };
      colors = {
        alpha = "0.93";
      };
    };
  };
}
