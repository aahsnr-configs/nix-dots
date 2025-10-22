# default.nix - Main entry point for Nixvim configuration
# This can be imported into your home-manager configuration
{ config, pkgs, lib, inputs, nixvim, ... }:

{
  imports = [
    inputs.nixvim.homeModules.nixvim    
    ./core.nix
    ./ui.nix
    ./lsp.nix
    ./plugins.nix
    ./keybindings.nix
  ];

  programs.nixvim = {
    enable = true;
    
    # Use the latest stable version
    viAlias = true;
    vimAlias = true;
    
    # Enable man pages for nixvim
    enableMan = true;
    
    # Default packages needed for various plugins
    extraPackages = with pkgs; [
      pyright
      nil
      nodePackages.bash-language-server
      nixpkgs-fmt
      nodePackages.prettier
      ruff
      ripgrep
      fd
      nodePackages.markdownlint-cli
    ];
  };
}
