{ pkgs, ... }:
{
  home.packages = with pkgs; [
    bitwarden
    fastfetch
    ferdium
    neovim
    nix-index
    nix-prefetch-git
    nix-prefetch-github
    qt5.qttools
    qt6Packages.qtstyleplugin-kvantum
    zotero
    inputs.quickshell.packages.${pkgs.system}.default
  ];
}
