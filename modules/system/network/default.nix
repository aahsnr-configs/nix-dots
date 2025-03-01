{ config, pkgs, lib, ...}:

{
  networking = {
    # dns
    networkmanager = {
      enable = true;
      unmanaged = ["docker0" "rndis0"];
      wifi = {
        macAddress = "random";
      };
    };
    firewall = {
      enable = true;
      allowedTCPPorts = [
        8081
        4321
      ];
      allowPing = false;
      logReversePathDrops = true;
      checkReversePath = "loose";
    };
  };

  # services.mullvad-vpn = {
  #   enable = true;
  #   enableExcludeWrapper = false;
  #   package = pkgs.mullvad-vpn;
  # };

  # slows down boot time
  systemd.services.NetworkManager-wait-online.enable = false;
}

