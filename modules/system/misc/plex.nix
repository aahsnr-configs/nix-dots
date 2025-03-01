{ config, pkgs, nixpkgs, ... }:

{
  services.plex = {
    enable = true;
    openFirewall = true;
    user="ahsan";
  };

}

