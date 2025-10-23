{
  inputs,
  pkgs,
  ...
}:
{
  imports = [ inputs.nix-doom-emacs-unstraightened.hmModule ];

  programs.doom-emacs = {
    enable = true;
    emacs = pkgs.emacs-unstable-pgtk;
    extraPackages = epkgs: [
      (epkgs.treesit-grammars.with-grammars (grammars: with grammars; [ 
        tree-sitter-bash
        tree-sitter-fish
        tree-sitter-python
        tree-sitter-nix
      ]))
    ];
    doomDir = ./doom.d;
    tangleArgs = "--all config.org";
    experimentalFetchTree = true;
  };

  # services.emacs = {
  #   enable = true;
  #   client.enable = true;
  #   socketActivation.enable = true;
  # };
}
