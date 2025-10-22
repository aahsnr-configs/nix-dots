# default.nix - Main entry point for Nixvim configuration
# This can be imported into your home-manager configuration
{ config, pkgs, lib, ... }:

{
  imports = [
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
      # Language servers
      pyright
      nil
      nodePackages.bash-language-server
      
      # Formatters
      black
      nixpkgs-fmt
      nodePackages.prettier
      
      # Linters
      ruff
      
      # Tools for telescope
      ripgrep
      fd
      
      # For markdown preview
      nodePackages.markdownlint-cli
    ];
  };
}
