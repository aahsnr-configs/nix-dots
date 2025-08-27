{ pkgs, ... }: {
  boot = {
    consoleLogLevel = 5;

    initrd = {
      verbose = false;
      systemd.enable = true;
      #luks.devices."luks-392510c6-7a4d-45fc-b6bd-4f12c5a2e6df".device = "/dev/disk/by-uuid/392510c6-7a4d-45fc-b6bd-4f12c5a2e6df";
    };

    kernelPackages = pkgs.linuxPackages_zen;

    kernelParams = [
      "quiet"
      "splash"
      "loglevel=3"
      "nowatchdog"
      "apparmor=1"
      "security=apparmor"
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

    # plymouth = {
    #   enable = true;
    #   theme = "square_hud";
    #   themePackages = with pkgs; [
    #     (adi1090x-plymouth-themes.override {
    #       selected_themes = [ "square_hud" ];
    #     })
    #   ];
    # };

    tmp = {
      cleanOnBoot = true;
      useTmpfs = false;
    };
  };
}
