{ pkgs, ... }:
{
  boot = {
    consoleLogLevel = 5;

    initrd = {
      verbose = false;
      systemd.enable = true;
      luks.devices."luks-69192390-b487-4dda-bdd1-59b7e9be1ef1".device = "/dev/disk/by-uuid/69192390-b487-4dda-bdd1-59b7e9be1ef1";
    };

    kernelPackages = pkgs.linuxPackages_xanmod_latest;

    kernelParams = [
      "quiet"
      "splash"
      "loglevel=3"
      "nowatchdog"
    ];

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };

      systemd-boot = {
        enable = true;
        editor = false;
        configurationLimit = 0;
      };

      timeout = 5;
    };

    plymouth = {
      enable = true;
    };

    tmp = {
      cleanOnBoot = true;
      useTmpfs = false;
    };
  };
}
