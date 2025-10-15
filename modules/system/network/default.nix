{ ... }:
{

  networking = {
    networkmanager = { 
      enable = true;
      wifi.powersave = true;
    };
    firewall = {
      enable = true;
      allowedTCPPorts = [
        8081
        4321
        47
      ];
      allowPing = false;
      logReversePathDrops = true;
      checkReversePath = "loose";
    };
  };

  # slows down boot time
  systemd.services.NetworkManager-wait-online.enable = false;
}
