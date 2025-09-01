{ pkgs, ... }:

{
  programs = {
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-dropbox-plugin
        thunar-media-tags-plugin
        thunar-volman
      ];
    };
    xfconf.enable = true;
  };

  services = {
    tumbler.enable = true;
    gvfs.enable = true;
  };
}
