{pkgs, ...}: {
  boot = {
    consoleLogLevel = 5;

    initrd = {
      verbose = false;
      systemd.enable = true;
      luks.devices."luks-f3d8460b-b00c-4eba-ac46-e1b8478a46c8".device = "/dev/disk/by-uuid/f3d8460b-b00c-4eba-ac46-e1b8478a46c8";
    };

    kernelPackages = pkgs.linuxPackages_xanmod_latest;

    kernelParams = [
      "splash"
      "loglevel=3"
      "nowatchdog"
      "amd_pstate=active"
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
      theme = "square_hud";
      themePackages = with pkgs; [
        # By default we would install all themes
        (adi1090x-plymouth-themes.override {
          selected_themes = ["square_hud"];
        })
      ];
    };

    tmp = {
      cleanOnBoot = true;
      useTmpfs = false;
    };
  };
}
