{ pkgs, ... }:
{
  home.packages = with pkgs; [
    bitwarden
    fastfetch
    ferdium
    nix-index
    nix-prefetch-git
    nix-prefetch-github
    qt5.qttools
    qt6Packages.qtstyleplugin-kvantum
    zotero
  ];
}
