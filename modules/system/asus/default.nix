{ pkgs, ... }: {
  services = {
    asusd = {
      enable = true;
      enableUserService = true;
    };
    supergfxd.enable = true;
  };

  systemd.services.supergfxd.path = [ pkgs.pciutils ];
}
