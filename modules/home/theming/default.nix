{
  config,
  pkgs,
  ...
}: {
  # gtk = {
    # enable = true;
    #
    # # cursorTheme = {
    # #   name = "Bibata-Modern-Ice";
    # #   size = 24;
    # #   package = pkgs.bibata-cursors;
    # # };
    #
    # # theme = {
    # #   package = pkgs.adw-gtk3;
    # #   name = "adw-gtk3";
    # # };
    # #
    # iconTheme = {
    #   package = pkgs.papirus-icon-theme;
    #   name = "Papirus-Dark";
    # };
    #
    # font = {
    #   name = "JetBrains Mono";
    #   size = 13;
    # };

    # gtk2.extraConfig = ''
    #   gtk-xft-antialias=1
    #   gtk-xft-hinting=1
    #   gtk-xft-hintstyle="hintslight"
    #   gtk-xft-rgba="rgb"
    # '';
    #
    # gtk3.bookmarks = [
    #   "file://${config.home.homeDirectory}/Dev"
    #   "file://${config.home.homeDirectory}/Documents"
    #   "file://${config.home.homeDirectory}/Downloads"
    #   "file://${config.home.homeDirectory}/Music"
    #   "file://${config.home.homeDirectory}/Pictures"
    #   "file://${config.home.homeDirectory}/Videos"
    # ];
    #
    # gtk3.extraConfig = {
    #   gtk-xft-antialias = 1;
    #   gtk-xft-hinting = 1;
    #   gtk-xft-hintstyle = "hintsfull";
    #   gtk-xft-rgba = "rgb";
    #   gtk-application-prefer-dark-theme = 1;
    # };
    #
    # gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
  # };

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      emoji = ["Noto color emoji"];
      monospace = ["JetBrains Mono"];
      sansSerif = ["Rubik Medium"];
      serif = [
        "Noto Serif"
        "Noto Color Emoji"
      ];
    };
    hinting = "slight";
    subpixelRendering = "rgb";
    antialiasing = true;
  };

  home.packages = with pkgs; [
    corefonts
    dina-font
    (google-fonts.override {fonts = ["Inter"];})
    jetbrains-mono
    liberation_ttf
    material-symbols
    mplus-outline-fonts.githubRelease
    nerd-fonts.jetbrains-mono
    nerd-fonts.symbols-only
    nerd-fonts.caskaydia-mono
    noto-fonts
    noto-fonts-color-emoji
    rubik
    powerline-fonts
    terminus_font
    ubuntu-classic
    ubuntu-sans
    vista-fonts
  ];

  # qt = {
  #   enable = true;
  #   platformTheme.name = "kde";
  #   style.name = "kvantum";
  # };

  # dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
}
