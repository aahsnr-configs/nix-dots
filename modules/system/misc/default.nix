{ pkgs, ... }:
{
  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-v32n.psf.gz";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "C.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
    ];
    extraLocaleSettings = {
      LC_ADDRESS = "en_US";
      LC_IDENTIFICATION = "en_US";
      LC_MEASUREMENT = "en_US";
      LC_MONETARY = "en_US";
      LC_NAME = "en_US";
      LC_NUMERIC = "en_US";
      LC_PAPER = "en_US";
      LC_TELEPHONE = "en_US";
      LC_TIME = "en_US";
    };
  };

  programs = {
    dconf.enable = true;
    fuse.userAllowOther = true;
    uwsm.enable = true;
    xwayland.enable = true;
  };

  services = {
    flatpak.enable = true;
    fstrim.enable = true;
    gnome = {
      glib-networking.enable = true;
      gnome-keyring.enable = true;
    };
    sysprof.enable = true;
    udisks2.enable = true;
    acpid.enable = true;
    thermald.enable = true;
    upower.enable = true;
    power-profiles-daemon.enable = true;
  };

  time = {
    timeZone = "Asia/Dhaka";
    hardwareClockInLocalTime = true;
  };

}
