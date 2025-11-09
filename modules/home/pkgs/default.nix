{pkgs, ...}: {
  home.packages = with pkgs; [
    bitwarden-desktop
    fastfetch
    ferdium
    nwg-look
    nix-index
    nix-prefetch-git
    nix-prefetch-github
    zotero
    adw-gtk3
    qt6Packages.qt6ct
    trash-cli
    obsidian
    bottom
    brave
    protonvpn-gui
    ticktick
    onlyoffice-desktopeditors
    papirus-icon-theme
    tasks
  ];
}
