{ ... }:
{
  networking = {
    # dns
    networkmanager = {
      enable = true;
      unmanaged = [
        "docker0"
        "rndis0"
      ];
      wifi = {
        macAddress = "random";
      };
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
