{
  inputs,
  pkgs,
  ...
}: {
  imports = [inputs.nix-doom-emacs-unstraightened.hmModule];

  programs.doom-emacs = {
    enable = true;
    emacs = pkgs.emacs-pgtk;
    extraPackages = epkgs: [
      (epkgs.treesit-grammars.with-grammars (
        grammars:
          with grammars; [
            tree-sitter-bash
            tree-sitter-fish
            tree-sitter-python
            tree-sitter-nix
          ]
      ))
      pkgs.emacsPackages.lsp-bridge
    ];
    doomDir = ./doom.d;
    tangleArgs = "--all config.org";
    experimentalFetchTree = true;
  };

  services.emacs = {
    enable = true;
  };
}
