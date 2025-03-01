{ ... }:

{
  services = {
    gnome = {
      glib-networking.enable = true;
      gnome-keyring.enable = true;
    };
    udisks2.enable = true;
    fstrim.enable = true;
    sysprof.enable = true;
  };

  programs = { 
    fuse.userAllowOther = true;
    dconf.enable = true;
    uwsm.enable = true;
    xwayland.enable = true;
  };
}

