{ pkgs, inputs, ... }:
{
  home.packages = with pkgs; [
    bitwarden
    fastfetch
    ferdium
    neovim
    nix-index
    nix-prefetch-git
    nix-prefetch-github
    zotero
    inputs.quickshell.packages.${pkgs.system}.default
  ];
}
