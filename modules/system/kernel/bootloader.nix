{pkgs, ...}: {
  boot = {
    consoleLogLevel = 5;

    initrd = {
      verbose = false;
      systemd.enable = true;
      luks.devices."luks-f3d8460b-b00c-4eba-ac46-e1b8478a46c8".device = "/dev/disk/by-uuid/f3d8460b-b00c-4eba-ac46-e1b8478a46c8";
    };

    kernelPackages = pkgs.linuxPackages_cachyos;

    kernelParams = [
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
        configurationLimit = 50;
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
