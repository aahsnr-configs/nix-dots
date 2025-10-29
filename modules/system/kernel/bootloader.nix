{pkgs, ...}: {
  boot = {
    consoleLogLevel = 5;

    initrd = {
      verbose = false;
      systemd.enable = true;
      luks.devices."luks-c931ab4a-8d18-4092-ae17-4cc82a7f9d14".device = "/dev/disk/by-uuid/c931ab4a-8d18-4092-ae17-4cc82a7f9d14";
    };

    kernelPackages = pkgs.linuxPackages_cachyos;

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
