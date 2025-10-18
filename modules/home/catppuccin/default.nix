# ~/.config/home-manager/catppuccin/default.nix
{ ... }:
{
  catppuccin = {
    atuin = {
      enable = true;
      flavor = "mocha";
      accent = "flamingo";
    };

    bat = {
      enable = true;
      flavor = "mocha";
    };

    bottom = {
      enable = true;
      flavor = "mocha";
    };

    brave = {
      enable = true;
      flavor = "mocha";
    };

    btop = {
      enable = true;
      flavor = "mocha";
    };

    delta = {
      enable = true;
      flavor = "mocha";
    };

    fish = {
      enable = true;
      flavor = "mocha";
    };

    foot = {
      enable = true;
      flavor = "mocha";
    };

    fzf = {
      enable = true;
      accent = "flamingo";
      flavor = "mocha";
    };

    imv = {
      enable = true;
      flavor = "mocha";
    };

    kitty = {
      enable = true;
      flavor = "mocha";
    };

    lazygit = {
      enable = true;
      accent = "flamingo";
      flavor = "mocha";
    };

    mpv = {
      enable = true;
      accent = "flamingo";
      flavor = "mocha";
    };

    tmux = {
      enable = true;
      extraConfig = ''
        set -g @catppuccin_status_modules_right "application session user host date_time"
      '';
      flavor = "mocha";
    };

    starship = {
      enable = true;
      flavor = "mocha";
    };

    yazi = {
      enable = true;
      accent = "flamingo";
      flavor = "mocha";
    };

    zathura = {
      enable = true;
      flavor = "mocha";
    };
  };
}
