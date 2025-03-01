{
  home = {
    username = "ahsan";
    homeDirectory = "/home/ahsan";
    stateVersion = "25.05";
    extraOutputsToInstall = ["doc" "info" "devdoc"];
    sessionVariables = {
      TERMINAL = "foot";
      BROWSER = "zen";
      EDITOR = "nvim";
    };

    # sessionPath = [
    #   "$HOME/nix-dots/scripts"
    # ];

  };

  programs.home-manager.enable = true;

  imports = [
    #./alacritty
    ./anyrun
    ./bat
    ./catppuccin
    ./cliphist
    ./dev
    ./dropbox
    ./emacs
    ./emoji
    ./eza
    ./fastfetch
    ./foot
    ./fzf
    ./git
    ./hypridle
    ./hyprland
    ./hyprlock
    ./hyprpaper
    ./hyprpanel
    ./imv
    ./kitty
    ./lazygit
    ./mpv
    # ./nwg-dock-hyprland
    ./nvim
    ./ripgrep
    ./starship
    ./tealdeer
    ./texlive
    ./theming
    ./thefuck
    ./tmux
    ./wofi
    ./xdg
    ./yazi
    ./zathura
    #./zellij
    ./zoxide
    ./zsh
  ];

  # nixpkgs = {
  #   config = {
  #     allowUnfree = true;
  #     allowBroken = true;
  #     allowUnfreePredicate = true;
  #   };
  # };
}
