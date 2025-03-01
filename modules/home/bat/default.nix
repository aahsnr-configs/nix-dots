{ pkgs, ... }:

{
  programs.bat = {
    enable = true;
    config = {
      #theme = "Catppuccin Macchiato";
      italic-text = "always";
      paging = "never";
      pager="less --RAW-CONTROL-CHARS --quit-if-one-screen --mouse";
      map-syntax = [
        "*.ino:C++"
        ".ignore:Git Ignore"
      ];
    };
    extraPackages = with pkgs.bat-extras; [
      batdiff 
      batman 
      batgrep 
      batwatch
      batpipe
    ];
  };
}
