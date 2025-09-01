{ pkgs, ... }:
{
  boot = {
    consoleLogLevel = 5;

    initrd = {
      verbose = false;
      systemd.enable = true;
      luks.devices."luks-374fa84a-a386-4666-bd07-6a87c6c853a8".device =
        "/dev/disk/by-uuid/374fa84a-a386-4666-bd07-6a87c6c853a8";
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
      theme = "bgrt";
    };

    tmp = {
      cleanOnBoot = true;
      useTmpfs = false;
    };
  };
}
