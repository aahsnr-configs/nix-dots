{ config, pkgs, ... }: 

{
  programs.gnupg.agent = {
    enable = true;
    #homedir = "${config.home.homeDirectory}/.gnupg";
    pinentryPackage = pkgs.pinentry-gnome3;
    enableSSHSupport = true;
    settings = {
      #default-key = "B820F6E378AC9A3D";
    };
  };
}

