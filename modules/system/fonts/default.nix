{pkgs, ...}: {
  fonts = {
    enableGhostscriptFonts = true;
    enableDefaultPackages = true;
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = ["Noto color emoji"];
        monospace = ["JetBrains Mono"];
        sansSerif = ["Rubik Regular"];
        serif = [
          "Noto Serif"
          "Noto Color Emoji"
        ];
      };
      hinting = {
        enable = true;
        autohint = true;
        style = "full";
      };
      subpixel.rgba = "rgb";
      antialias = true;
    };

    packages = with pkgs; [
      corefonts
      (google-fonts.override {fonts = ["Inter"];})
      jetbrains-mono
      material-symbols
      nerd-fonts.jetbrains-mono
      nerd-fonts.sauce-code-pro
      noto-fonts
      noto-fonts-color-emoji
      rubik
      terminus_font
      source-code-pro
      cascadia-code
    ];
  };
}
