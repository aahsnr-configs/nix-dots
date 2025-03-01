{ config, pkgs, ... }:

{
  gtk = {
    enable = true;

    theme = {
      package = pkgs.magnetic-catppuccin-gtk.override {
        accent = [ "orange" ];
        size = "compact";
        tweaks = [ "macchiato" ];
      };
      name = "Catppuccin-GTK-Orange-Dark-Compact-Macchiato";
    };


    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };

    font = {
      name = "JetBrains Mono";
      size = 13;
    };

    gtk2.extraConfig = ''
      gtk-xft-antialias=1
      gtk-xft-hinting=1
      gtk-xft-hintstyle="hintslight"
      gtk-xft-rgba="rgb"
    '';

    gtk3.bookmarks = [
      "file://${config.home.homeDirectory}/Dev"
      "file://${config.home.homeDirectory}/Documents"
      "file://${config.home.homeDirectory}/Downloads"
      "file://${config.home.homeDirectory}/Music"
      "file://${config.home.homeDirectory}/Pictures"
      "file://${config.home.homeDirectory}/Videos"
    ];

    gtk3.extraConfig = {
      gtk-xft-antialias = 1;
      gtk-xft-hinting = 1;
      gtk-xft-hintstyle = "hintsfull";
      gtk-xft-rgba = "rgb";
      gtk-application-prefer-dark-theme = 1;
    };

    gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;


  };

  home = {
    packages = with pkgs; [
      qt5.qttools
      qt6Packages.qtstyleplugin-kvantum
      libsForQt5.qtstyleplugin-kvantum
      libsForQt5.qt5ct
      kdePackages.qt6ct
    ];

    pointerCursor = {
      package = pkgs.pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
      size = 32;
      gtk.enable = true;
      #x11.enable = true;
    };

    sessionVariables = {
      XCURSOR_SIZE = "32";
      GTK_USE_PORTAL = "1";
    };
  };

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style.name = "kvantum";
  };

   dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";

}
