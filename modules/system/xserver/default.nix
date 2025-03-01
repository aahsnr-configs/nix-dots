{ pkgs, ... }:

{
  services.xserver = {
    enable = false;
    xkb = {
      layout = "us";
      variant = "";
    };
    dpi = 250;
    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
    desktopManager.gnome.enable = true;
  };

  environment.gnome.excludePackages = with pkgs; [
    orca
    cantarell-fonts
    evince
    file-roller
    geary
    gnome-disk-utility
    seahorse
    sushi
    gnome-shell-extensions
    adwaita-fonts
    adwaita-icon-theme
    #nixos-background-info
    gnome-backgrounds
    gnome-bluetooth
    gnome-color-manager
    gnome-control-center
    gnome-shell-extensions
    gnome-remote-desktop
    gnome-tour # GNOME Shell detects the .desktop file on first log-in.
    gnome-user-docs
    gnome-menus
    gtk3.out # for gtk-launch program
    baobab
    epiphany
    gnome-text-editor
    gnome-calculator
    gnome-calendar
    gnome-characters
    gnome-clocks
    gnome-console
    gnome-contacts
    gnome-font-viewer
    gnome-logs
    gnome-maps
    gnome-music
    gnome-system-monitor
    gnome-weather
    loupe
    nautilus
    gnome-connections
    simple-scan
    snapshot
    totem
    yelp
    gnome-software
    xdg-desktop-portal-gnome
  ];
}

