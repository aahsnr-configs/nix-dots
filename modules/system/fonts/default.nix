{pkgs, ...}: {
  fonts = {
    fontconfig = {
      enable = true;
      antialias = true;
      hinting = {
        enable = true;
        autohint = true;
        style = "full";
      };

      subpixel = {
        lcdfilter = "default";
        rgba = "rgb";
      };

      defaultFonts = {
        monospace = ["JetBrainsMono Nerd Font"];
        sansSerif = ["Geist" "Noto Color Emoji"];
        serif = ["Noto Serif" "Noto Color Emoji"];
        emoji = ["Noto Color Emoji"];
      };
    };

    fontDir = {
      enable = true;
      decompressFonts = true;
    };

    enableDefaultPackages = true;

    packages = with pkgs; [
      corefonts
      dina-font
      fira-code
      fira-code-symbols
      (google-fonts.override {fonts = ["Inter"];})
      jetbrains-mono
      liberation_ttf
      material-symbols
      mplus-outline-fonts.githubRelease
      nerd-fonts.jetbrains-mono
      nerd-fonts.ubuntu
      nerd-fonts.ubuntu-mono
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      powerline-fonts
      proggyfonts
      rubik
      terminus_font
      ubuntu_font_family
      vistafonts
    ];
  };
}
