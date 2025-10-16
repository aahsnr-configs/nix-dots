# ~/.config/home-manager/dev/default.nix
{ pkgs, ... }:
{
  home.packages = with pkgs; [
    alejandra
    deadnix
    delta
    fd
    nil
    nixfmt
    nixpkgs-fmt
    nodejs_24
    lua5_1
    luarocks
    ripgrep
    statix
    marksman
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    # not needed
    #enableFishIntegration = true;
    config.global.hide_env_diff = true;
  };
}
