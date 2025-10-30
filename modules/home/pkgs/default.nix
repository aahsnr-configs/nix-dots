{
  pkgs,
  inputs,
  ...
}: {
  # imports = [
  #   inputs.caelestia-shell.homeManagerModules.default
  # ];
  home.packages = with pkgs; [
    bitwarden-desktop
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
    # inputs.caelestia-cli.packages.${system}.default
    upscayl
    qt6Packages.qt6ct
    chafa
    trash-cli
    obsidian
    bottom
    brave
    protonvpn-gui
    ticktick
    onlyoffice-desktopeditors
    todoist-electron
    firefox
  ];
}
