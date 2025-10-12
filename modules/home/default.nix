{ pkgs, ... }:
{
  home = {
    username = "ahsan";
    homeDirectory = "/home/ahsan";
    stateVersion = "25.11";
    extraOutputsToInstall = [
      "doc"
      "info"
      "devdoc"
    ];

    # Set default editor and other environment variables
    sessionVariables = {
      BROWSER = "zen-browser";
      TERMINAL = "footclient";
      EDITOR = "emacsclient -t -a 'emacs'";
      VISUAL = "emacsclient -c -a 'emacs'";
    };

    pointerCursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Ice";
      size = 32;
      gtk.enable = true;
      x11.enable = true;
    };

    shell.enableFishIntegration = true;

    # Consolidate PATH from export.zsh
    sessionPath = [
      "$HOME/.cargo/bin"
      "$HOME/go/bin"
      "$HOME/.bun/bin"
      "$HOME/.local/bin"
      "$HOME/.local/bin/hypr"
      "$HOME/.config/emacs/bin"
      "$HOME/.npm-global/bin"
      "$HOME/.local/share/flatpak/exports/bin"
    ];
  };

  imports = [
    #./anyrun
    ./atuin
    ./bat
    ./btop
    ./caelestia
    ./catppuccin
    ./cliphist
    ./dev
    ./eza
    ./fd-find
    ./fish
    ./foot
    ./fzf
    ./git
    ./hyprland
    ./hyprpolkitagent
    ./hyprsunset
    ./imv
    ./keyring
    ./kitty
    ./lazygit
    ./mpv
    ./pay-respects
    ./pkgs
    ./ripgrep
    ./starship
    ./texlive
    ./tldr
    ./tmux
    ./udiskie
    ./xdg
    ./yazi
    ./zathura
    ./zen-browser
    ./zoxide
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
