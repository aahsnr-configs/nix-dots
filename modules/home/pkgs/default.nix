{ pkgs, inputs, ... }:
{
  home.packages = with pkgs; [
    bitwarden
    fastfetch
    ferdium
    nwg-look
    nix-index
    nix-prefetch-git
    nix-prefetch-github
    zotero
    inputs.quickshell.packages.${pkgs.system}.default
    inputs.hyprsunset.packages.${pkgs.system}.hyprsunset
    inputs.hyprpolkitagent.packages.${pkgs.system}.hyprpolkitagent
    upscayl
    qt6ct
    chafa
    trash-cli
    obsidian
    bottom
    brave
    protonvpn-gui
    ticktick
    onlyoffice-desktopeditors
    todoist-electron
    celluloid
  ];
}
