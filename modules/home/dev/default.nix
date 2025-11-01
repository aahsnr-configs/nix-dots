# ~/.config/home-manager/dev/default.nix
{pkgs, ...}: {
  home.packages = with pkgs; [
    alejandra
    deadnix
    delta
    nil
    nixfmt
    nixpkgs-fmt
    nodejs_24
    lua5_1
    luarocks
    statix
    marksman
    pandoc
    gnuplot
    hunspell
    hunspellDicts.en_US
    gnuplot
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    config.global.hide_env_diff = true;
  };
}
