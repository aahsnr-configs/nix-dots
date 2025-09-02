{pkgs, ...}: {
  home = {
    username = "ahsan";
    homeDirectory = "/home/ahsan";
    stateVersion = "25.11";
    extraOutputsToInstall = ["doc" "info" "devdoc"];

    # Set default editor and other environment variables
    sessionVariables = {
      EDITOR = "nvim";
      BROWSER = "zen-browser";
      VISUAL = "emacsclient -c -a 'emacs'";
      TERMINAL = "kitty";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      PAGER = "bat --paging=always --style=plain";
      LESS = "-R --use-color -Dd+r -Du+b -DS+s -DE+g";
      LANG = "en_US.UTF-8";
      XCURSOR_SIZE = "32";
      GTK_USE_PORTAL = "1";
    };

    pointerCursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Ice";
      size = 32;
      gtk.enable = true;
      x11.enable = true;
    };

    shellAliases.vi = "nvim";

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
    ./anyrun
    ./atuin
    ./bat
    ./btop
    ./caelestia
    ./catppuccin
    ./cliphist
    ./dev
    ./emacs
    ./emoji
    ./eza
    ./fd-find
    ./fzf
    ./git
    ./hypridle
    ./hyprland
    ./hyprlock
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
    ./schizofox
    ./starship
    ./texlive
    ./theming
    ./tldr
    ./tmux
    #./vscode
    ./xdg
    ./yazi
    ./zathura
    ./zen-browser
    ./zoxide
    ./zsh
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
