{ pkgs, ... }: {
  home.packages = with pkgs; [
    alejandra
    brightnessctl
    delta
    emacs-lsp-booster
    fastfetch
    kdePackages.qt6ct
    libsForQt5.qt5ct
    libsForQt5.qtstyleplugin-kvantum
    lua5_1
    luarocks
    nil
    nix-index
    nix-prefetch-git
    nix-prefetch-github
    nixfmt
    nixpkgs-fmt
    nodejs_24
    python313Packages.kde-material-you-colors
    qt5.qttools
    qt6Packages.qtstyleplugin-kvantum
    textlint
    zotero
  ];
}
